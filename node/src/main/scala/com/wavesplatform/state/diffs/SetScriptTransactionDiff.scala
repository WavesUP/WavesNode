package com.wavesplatform.state.diffs

import shapeless.syntax.std.tuple._
import cats.implicits._
import com.wavesplatform.features.EstimatorProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.SetScriptTransaction

object SetScriptTransactionDiff {
  def apply(blockchain: Blockchain)(tx: SetScriptTransaction): Either[ValidationError, Diff] =
    for {
      callableComplexities <- tx.script match {
        case Some(ContractScriptImpl(v, dApp)) =>
          val callables = dApp.copy(verifierFuncOpt = None)
          ContractScript.estimateComplexity(v, callables, blockchain.estimator).leftMap(GenericError(_))
        case _ =>
          Right((0L, Map[String, Long]()))
      }
      verifierWithComplexity <- DiffsCommon.countVerifierComplexity(tx.script, blockchain)
      scriptWithComplexities = verifierWithComplexity.map(s => tx.sender +: s :+ callableComplexities._2)
    } yield Diff(
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
        scripts = Map(tx.sender.toAddress -> scriptWithComplexities),
        scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)
      )
}
