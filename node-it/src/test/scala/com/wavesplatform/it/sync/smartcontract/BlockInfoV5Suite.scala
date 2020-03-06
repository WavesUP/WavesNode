package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.common.utils.EitherExt2

class BlockInfoV5Suite extends BaseTransactionSuite {
  val activationHeight = 3

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.Builder(Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((15, activationHeight)))
      .buildNonConflicting()

  private val dAppScriptV4 =
    """{-# STDLIB_VERSION 4 #-}
      |{-# CONTENT_TYPE DAPP #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}
      |
      |@Callable(i)
      |func blockInfo(h: Int) = {
      |    let vrf = match blockInfoByHeight(h) {
      |        case block: BlockInfo => block.generationSignature
      |        case _ => throw("can't find block")
      |    }
      |
      |    BinaryEntry("vrf", vrf)
      |
      |}
      |""".stripMargin

  private val dAppScriptV3 =
    """{-# STDLIB_VERSION 3 #-}
      |{-# CONTENT_TYPE DAPP #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}
      |
      |@Callable(i)
      |func blockInfo(h: Int) = {
      |    let vrf = match blockInfoByHeight(h) {
      |        case block: BlockInfo => block.vrf
      |        case _ => throw("can't find block")
      |    }
      |
      |    WriteSet([DataEntry("vrf", vrf)])
      |
      |}
      |""".stripMargin

  private val caller = firstAddress
  private val dApp = secondAddress
  test("able to retrieve vrf from block V5 in RideV4") {
    val script = ScriptCompiler.compile(dAppScriptV4, ScriptEstimatorV3).explicitGet()._1.bytes().base64
    sender.setScript(dApp, Some(script), waitForTx = true)

    sender.invokeScript(caller, dApp, func = Some("blockInfo"), args = List(Terms.CONST_LONG(3)), waitForTx = true)

    println(sender.getDataByKey(dApp, "vrf"))

  }
}
