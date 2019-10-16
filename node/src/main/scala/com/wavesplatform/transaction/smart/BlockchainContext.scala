package com.wavesplatform.transaction.smart

import cats.kernel.Monoid
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{ContentType, ScriptType, StdLibVersion}
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.{ExecutionError, Global}
import com.wavesplatform.state._
import monix.eval.Coeval

object BlockchainContext {

  type In = WavesEnvironment.In
  def build(version: StdLibVersion,
            nByte: Byte,
            in: Coeval[Environment.InputEntity],
            h: Coeval[Int],
            blockchain: Blockchain,
            isTokenContext: Boolean,
            isContract: Boolean,
            address: Coeval[ByteStr]): Either[ExecutionError, EvaluationContext] =
    DirectiveSet(
      version,
      ScriptType.isAssetScript(isTokenContext),
      ContentType.isDApp(isContract)
    ).map(ds => WavesContext.build(ds, new WavesEnvironment(nByte, in, h, blockchain, address, ds)))
      .map(Seq(PureContext.build(Global, version), CryptoContext.build(Global, version), _))
      .map(Monoid.combineAll(_))
      .map(_.evaluationContext)
}
