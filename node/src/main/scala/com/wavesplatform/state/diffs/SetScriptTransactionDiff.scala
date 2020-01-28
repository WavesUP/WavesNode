package com.wavesplatform.state.diffs

import cats.implicits._
import com.wavesplatform.features.EstimatorProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.state.{AccountScriptInfo, Blockchain, Diff, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.SetScriptTransaction

object SetScriptTransactionDiff {
  def apply(blockchain: Blockchain)(tx: SetScriptTransaction): Either[ValidationError, Diff] =
    for {
      callableComplexities <- tx.script match {
        case Some(ContractScriptImpl(version, dApp)) => estimate(blockchain, version, dApp)
        case _                                       => Right(Map[Int, Map[String, Long]]())
      }
      verifierWithComplexity <- DiffsCommon.countVerifierComplexity(tx.script, blockchain)
      scriptWithComplexities = verifierWithComplexity.map { case (script, maxComplexity) =>
          AccountScriptInfo(tx.sender, script, maxComplexity, callableComplexities)
      }
    } yield Diff(
      tx = tx,
      portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
      scripts    = Map(tx.sender.toAddress    -> scriptWithComplexities),
      scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)
    )

  private def estimate(blockchain: Blockchain, version: StdLibVersion, dApp: DApp) = {
    val callables = dApp.copy(verifierFuncOpt = None)
    val actualComplexities =
      for {
        currentComplexity <- ContractScript.estimateComplexity(version, callables, blockchain.estimator)
        nextComplexities  <- estimateNext(blockchain, version, callables)
        complexitiesByEstimator =
          (currentComplexity :: nextComplexities)
            .mapWithIndex { case ((_, complexitiesByCallable), i) => (i + blockchain.estimator.version, complexitiesByCallable) }
            .toMap
      } yield complexitiesByEstimator

    actualComplexities.leftMap(GenericError(_))
  }

  private def estimateNext(blockchain: Blockchain, version: StdLibVersion, dApp: DApp) =
    ScriptEstimator.all
      .drop(blockchain.estimator.version)
      .traverse(se => ContractScript.estimateComplexity(version, dApp, se, checkLimit = false))
}
