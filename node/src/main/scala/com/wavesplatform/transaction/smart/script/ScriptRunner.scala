package com.wavesplatform.transaction.smart.script

import cats.implicits._
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.MultiPaymentPolicyProvider._
import com.wavesplatform.lang._
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, TRUE}
import com.wavesplatform.lang.v1.evaluator.{EvaluatorV1, _}
import com.wavesplatform.state._
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart._
import com.wavesplatform.transaction.{Authorized, Proven}
import monix.eval.Coeval
import shapeless.Inl

object ScriptRunner {
  type TxOrd = BlockchainContext.In

  def apply(in: TxOrd,
            blockchain: Blockchain,
            script: Script,
            isAssetScript: Boolean,
            scriptContainerAddress: ByteStr): (Log, Either[ExecutionError, EVALUATED]) =
    script match {
      case s: ExprScript =>
        val paymentCheck = in match {
          case Inl(ist: InvokeScriptTransaction) =>
            InvokeScriptTransaction.checkPayments(
              ist,
              blockchain.multiPaymentAllowed,
              s.stdLibVersion >= V4
            )
          case _ => Right(())
        }
        lazy val ctx = BlockchainContext.build(
          script.stdLibVersion,
          AddressScheme.current.chainId,
          Coeval.evalOnce(in),
          Coeval.evalOnce(blockchain.height),
          blockchain,
          isAssetScript,
          false,
          Coeval(scriptContainerAddress)
        )
        EvaluatorV1.applyWithLogging[EVALUATED](paymentCheck.flatMap(_ => ctx), s.expr)
      case ContractScript.ContractScriptImpl(_, DApp(_, decls, _, Some(vf), version)) =>
        val ctx = BlockchainContext.build(
          script.stdLibVersion,
          AddressScheme.current.chainId,
          Coeval.evalOnce(in),
          Coeval.evalOnce(blockchain.height),
          blockchain,
          isAssetScript,
          true,
          Coeval(scriptContainerAddress)
        )
        val evalContract = in.eliminate(
          t => ContractEvaluator.verify(decls, vf, RealTransactionWrapper(t, blockchain.multiPaymentAllowed, version)),
          _.eliminate(t => ContractEvaluator.verify(decls, vf, RealTransactionWrapper.ord(t)), _ => ???)
        )
        EvaluatorV1.evalWithLogging(ctx, evalContract)

      case ContractScript.ContractScriptImpl(_, DApp(_, _, _, None, _)) =>
        val t: Proven with Authorized =
          in.eliminate(_.asInstanceOf[Proven with Authorized], _.eliminate(_.asInstanceOf[Proven with Authorized], _ => ???))
        (List.empty, Verifier.verifyAsEllipticCurveSignature[Proven with Authorized](t) match {
          case Right(_)                => Right(TRUE)
          case Left(GenericError(err)) => Left(err)
        })
      case _ => (List.empty, "Unsupported script version".asLeft[EVALUATED])
    }
}
