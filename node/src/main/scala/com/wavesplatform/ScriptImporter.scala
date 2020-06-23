package com.wavesplatform

import java.io.{BufferedWriter, File, FileWriter}
import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}

import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.{Keys, LevelDBWriter, openDB}
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{EXPR, FUNC}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.state.{Height, TxNum}
import com.wavesplatform.transaction.assets.SetAssetScriptTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import monix.eval.Coeval
import monix.execution.UncaughtExceptionReporter
import monix.reactive.Observer
import org.iq80.leveldb.DB

import scala.collection.mutable

object ScriptImporter extends App {
  val settings = Application.loadApplicationConfig(Some(new File("/etc/waves/waves.conf")).filter(_.exists()))

  AddressScheme.current = new AddressScheme { override val chainId: Byte = 'W' }

  val db: DB = openDB(settings.dbSettings.directory)

  val levelDBWriter: LevelDBWriter =
    new LevelDBWriter(
      db,
      Observer.empty(UncaughtExceptionReporter.default),
      settings.blockchainSettings,
      settings.dbSettings
    )

  println(s"Height: ${levelDBWriter.height}")

  lazy val costs: Map[StdLibVersion, Map[FunctionHeader, Coeval[Long]]] =
    List(V1, V2, V3).map(v => (v, functionCosts(v))).toMap

  lazy val updatedCosts: Map[StdLibVersion, Map[FunctionHeader, Coeval[Long]]] =
    costs.map {
      case (version, costs) =>
        version ->
          costs.map {
            case (header, currentCost) =>
              header ->
                lazyContexts(DirectiveSet(V4, Account, Expression).explicitGet())
                  .value()
                  .evaluationContext(environment)
                  .functions
                  .get(header)
                  .flatMap(_.costByLibVersion.get(V4))
                  .map(Coeval.now)
                  .getOrElse(currentCost)
          }
    }

  val writer = new BufferedWriter(new FileWriter("all-updated.csv"))
  writer.write("address;type;current cost;new cost\n")

  val scripts: ConcurrentHashMap[(Address, String), (Long, Long)] = new ConcurrentHashMap[(Address, String), (Long, Long)]()

  (1190000 to levelDBWriter.height).par.foreach { height =>
    if (height % 10000 == 0) println(height)

    val txCount = db.get(Keys.blockHeaderAndSizeAt(Height(height))).map(_._1.transactionCount).getOrElse(0)
    (1 to txCount).flatMap(
      i =>
        db.get(Keys.transactionAtOpt(Height(height), TxNum(i.toShort))).flatten
          .collect {
            case (SetScriptTransaction(_, sender, Some(script), _, timestamp, _)) =>
              val address = sender.toAddress
              script match {
                case ContractScript.ContractScriptImpl(stdLibVersion, dApp) =>
                  dApp.verifierFuncOpt
                    .foreach { verifier =>
                      val (currentCost, newCost) = estimateAnnotatedFunc(
                        dApp,
                        stdLibVersion,
                        Some(verifier.annotation.invocationArgName),
                        verifier.u
                      )
                      scripts put((address, "verifier"), (currentCost, newCost))
                    }

                  dApp.callableFuncs
                    .foreach { callable =>
                      val (currentCost, newCost) = estimateAnnotatedFunc(
                        dApp,
                        stdLibVersion,
                        Some(callable.annotation.invocationArgName),
                        callable.u
                      )
                      scripts put((address, "callable"), (currentCost, newCost))
                    }

                case script: ExprScript =>
                  val (currentCost, newCost) = estimateExpr(script)
                  scripts put ((address, "verifier"), (currentCost, newCost))
                case _ =>
                  ???
              }
            case (SetAssetScriptTransaction(_, sender, _, Some(script: ExprScript), _, timestamp, _)) =>
              val address = sender.toAddress
              val (currentCost, newCost) = estimateExpr(script)
              scripts put ((address, "asset"), (currentCost, newCost))
          }
    )
  }

  scripts
    .forEach {
      case ((address, scriptType), (currentCost, newCost)) =>
        writer.write(s"$address;$scriptType;$currentCost;$newCost\n")
    }

  writer.close()

  private def estimateAnnotatedFunc(
      dApp: DApp,
      version: StdLibVersion,
      annotationArg: Option[String],
      function: FUNC
  ): (Long, Long) =
    estimate(ContractScript.constructExprFromFuncAndContext(dApp.decs, annotationArg, function), version)

  private def estimateExpr(script: ExprScript): (Long, Long) =
    estimate(script.expr, script.stdLibVersion)

  private def estimate(expr: EXPR, version: StdLibVersion): (Long, Long) = {
    val currentCost = ScriptEstimatorV3(Set(), costs(version), expr).explicitGet()
    val newCost     = ScriptEstimatorV3(Set(), updatedCosts(version), expr).explicitGet()
    (currentCost, newCost)
  }
}
