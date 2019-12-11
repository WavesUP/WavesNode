package com.wavesplatform.it.sync.block

import com.typesafe.config.Config
import com.wavesplatform.account.KeyPair._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.issueFee
import com.wavesplatform.it.transactions.BaseTransactionSuiteLike
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest._

class BlockComplexityLimitTestSuite extends FreeSpec with BaseTransactionSuiteLike {

  private val activationHeight   = 4
  private val minerDesiredReward = 750000000
  private val minIncrement       = 50000000
  private val initialReward      = 600000000
  private val rewardTerm         = 3
  private val votingInterval     = 2

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(
        _.raw(
          s"""waves {
          |blockchain.custom.genesis.average-block-delay = 30s
         |  miner.quorum = 0
         |  miner.micro-block-interval = 1s
         |  miner.min-micro-block-age = 0s
         |}""".stripMargin
        )
      )
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()

  private val dApp      = pkByAddress(firstAddress)
  private val caller    = pkByAddress(secondAddress)
  private val simpleAcc = pkByAddress(thirdAddress)



  private val nodeAddresses = nodeConfigs.map(_.getString("address")).toSet

  "_issue and transfer smart assets between dApp and caller" in {

    val estimator = ScriptEstimatorV2
    val simpleAsset =
      sender.issue(simpleAcc.stringRepr, "Asset1", "test asset", 1500000, 5, reissuable = true, issueFee, script = None, waitForTx = true).id

    val asset1 = sender
      .issue(
        caller.stringRepr,
        "Asset1",
        "test asset",
        1500000,
        5,
        reissuable = true,
        issueFee,
        script = Some(
          ScriptCompiler
            .compile(
              s"""
                 |{-# STDLIB_VERSION 4 #-}
                 |{-# CONTENT_TYPE EXPRESSION #-}
                 |{-# SCRIPT_TYPE ASSET #-}
                 |
                 |let proof = base58'3YtHbbHfeZC18tg63zAqhWWdoiD5uyqhzpsmcinPShkwB5oc4B47BkPLtcVMqSMeAZdiCZpSadn6i57kAYE93Ya4'
                 |let pub = base58'4G52s9NEwEBEHmp71qxc9cy8eE5agrjsEp7XyRWrmu3v'
                 |let body = base58'3HR3WSL4WtTn3765KCGQuGpFUzEdmmfdHwF'
                 |
                 |sigVerify(body, proof, pub) &&
                 |sigVerify(body, proof, pub) &&
                 |sigVerify(body, proof, pub) &&
                 |sigVerify(body, proof, pub) &&
                 |sigVerify(body, proof, pub) &&
                 |sigVerify(body, proof, pub) &&
                 |sigVerify(body, proof, pub) &&
                 |sigVerify(body, proof, pub) &&
                 |sigVerify(body, proof, pub) &&
                 |sigVerify(body, proof, pub) &&
                 |sigVerify(body, proof, pub) &&
                 |sigVerify(body, proof, pub) &&
                 |sigVerify(body, proof, pub) &&
                 |sigVerify(body, proof, pub) &&
                 |sigVerify(body, proof, pub) &&
                 |sigVerify(body, proof, pub) &&
                 |sigVerify(body, proof, pub) &&
                 |sigVerify(body, proof, pub) &&
                 |sigVerify(body, proof, pub)
                 |

                 |""".stripMargin,
              estimator
            )
            .explicitGet()
            ._1
            .bytes
            .value
            .base64
        ),
        waitForTx = true
      )
      .id

    val tx = sender.transfer(caller.stringRepr, dApp.toAddress.stringRepr, 1, 500000, Some(asset1), None, waitForTx = true).id

    val senderSrcript = ScriptCompiler
      .compile(
        s"""
           |{-# STDLIB_VERSION 4 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |
           |let pub = base58'4G52s9NEwEBEHmp71qxc9cy8eE5agrjsEp7XyRWrmu3v'
           |let proof = base58'3YtHbbHfeZC18tg63zAqhWWdoiD5uyqhzpsmcinPShkwB5oc4B47BkPLtcVMqSMeAZdiCZpSadn6i57kAYE93Ya4'
           |let body = base58'3HR3WSL4WtTn3765KCGQuGpFUzEdmmfdHwF'
           |
           |@Verifier(tx)
           |func sigVer() = {
           |  sigVerify(body, proof, pub)
           |  && sigVerify(body, proof, pub)
           |  && sigVerify(body, proof, pub)
           |  && sigVerify(body, proof, pub)
           |  && sigVerify(body, proof, pub)
           |  && sigVerify(body, proof, pub)
           |  && sigVerify(body, proof, pub)
           |  && sigVerify(body, proof, pub)
           |  && sigVerify(body, proof, pub)
           |  && sigVerify(body, proof, pub)
           |  && sigVerify(body, proof, pub)
           |  && sigVerify(body, proof, pub)
           |  && sigVerify(body, proof, pub)
           |  && sigVerify(body, proof, pub)
           |  && sigVerify(body, proof, pub)
           |  && sigVerify(body, proof, pub)
           |}

    """.stripMargin,
        estimator
      )
      .explicitGet()
      ._1

    val dAppScript = ScriptCompiler
      .compile(
        s"""
         |{-# STDLIB_VERSION 4 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |
         |@Callable(inv)
         |func segVer() = {
         |
         |    let pubKey = base58'4G52s9NEwEBEHmp71qxc9cy8eE5agrjsEp7XyRWrmu3v'
         |    let proof =  base58'3YtHbbHfeZC18tg63zAqhWWdoiD5uyqhzpsmcinPShkwB5oc4B47BkPLtcVMqSMeAZdiCZpSadn6i57kAYE93Ya4'
         |    let source = base58'3HR3WSL4WtTn3765KCGQuGpFUzEdmmfdHwF'
         |    let pmt = inv.payments[0]
         |
         |    [BooleanEntry("result",sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    && sigVerify(source,proof,pubKey)
         |    ),
         |
         |      ScriptTransfer(inv.caller, 1, pmt.assetId),
         |      ScriptTransfer(inv.caller, 1, pmt.assetId),
         |      ScriptTransfer(inv.caller, 1, pmt.assetId),
         |      ScriptTransfer(inv.caller, 1, pmt.assetId),
         |      ScriptTransfer(inv.caller, 1, pmt.assetId),
         |      ScriptTransfer(inv.caller, 1, pmt.assetId),
         |      ScriptTransfer(inv.caller, 1, pmt.assetId),
         |      ScriptTransfer(inv.caller, 1, pmt.assetId),
         |      ScriptTransfer(inv.caller, 1, pmt.assetId),
         |      ScriptTransfer(inv.caller, 1, pmt.assetId)
         |     ]
         |
         |}
         |@Verifier(tx)
         |func v() = {
         |    true
         |}
    """.stripMargin,
        estimator
      )
      .explicitGet()
      ._1

    val dAppSetScriptTxId = sender.setScript(dApp.stringRepr, Some(dAppScript.bytes().base64), waitForTx = true).id

    val start = System.currentTimeMillis()
    (1 to 500).foreach {
      case i @ x if i % 50 == 0 =>
        val blocksize     = sender.lastBlockHeaders.transactionCount
        val utxsize       = sender.utxSize
        val currentHeight = sender.height
        if ((utxsize > 100) && (blocksize > 35)) {
          val transferTx = sender.transfer(simpleAcc.stringRepr, caller.stringRepr, 1, 100000, Some(simpleAsset), None).id
          sender.waitForHeight(currentHeight + 1)
          sender.waitForTransaction(transferTx)
        }
      case _ =>
        sender.invokeScript(
          caller.stringRepr,
          dApp.stringRepr,
          Some("segVer"),
          payment = Seq(Payment(20, IssuedAsset(ByteStr.decodeBase58(asset1).get)), Payment(20, IssuedAsset(ByteStr.decodeBase58(asset1).get))),
          fee = 0.06.waves
        )
    }

  }
}
