package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.{ENOUGH_AMT, assertDiffAndState}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.assets.IssueTransactionV2
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class CallableV4DiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB with Inside {
  property("reissue and burn actions results state") {
    forAll(paymentPreconditions) {
      case (genesis, setScript, invoke, issue, master, reissueAmount, burnAmount) =>
        assertDiffAndState(
          Seq(TestBlock.create(genesis :+ setScript :+ issue)),
          TestBlock.create(Seq(invoke)),
          features
        ) { case (_, blockchain) =>
          val asset = IssuedAsset(issue.id.value)
          val resultAmount = issue.quantity + reissueAmount - burnAmount

          blockchain.assetDescription(asset).get.totalVolume shouldBe resultAmount
          blockchain.balance(master, asset) shouldBe resultAmount
        }
    }
  }

  private def paymentPreconditions: Gen[(List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, IssueTransactionV2, KeyPair, Long, Long)] =
    for {
      master  <- accountGen
      invoker <- accountGen
      ts      <- timestampGen
      fee     <- ciFee(1)
      issue   <- smartIssueTransactionGen(master, Gen.const(None), forceReissuable = true)
      reissueAmount <- positiveLongGen
      burnAmount    <- Gen.choose(0, reissueAmount)
    } yield {
      for {
        genesis     <- GenesisTransaction.create(master, ENOUGH_AMT, ts)
        genesis2    <- GenesisTransaction.create(invoker, ENOUGH_AMT, ts)
        setDApp     <- SetScriptTransaction.selfSigned(master, Some(dApp(issue.id.value, reissueAmount, burnAmount)), fee, ts + 2)
        ci <- InvokeScriptTransaction.selfSigned(invoker, master, None, Nil, fee, Waves, ts + 3)
      } yield (List(genesis, genesis2), setDApp, ci, issue, master, reissueAmount, burnAmount)
    }.explicitGet()

  private def dApp(assetId: ByteStr, reissueAmount: Long, burnAmount: Long): Script = {
    val script =
      s"""
         | {-# STDLIB_VERSION 4       #-}
         | {-# CONTENT_TYPE   DAPP    #-}
         | {-# SCRIPT_TYPE    ACCOUNT #-}
         |
         | @Callable(i)
         | func default() =
         |  [
         |    Reissue(base58'$assetId', true, $reissueAmount),
         |    Burn(base58'$assetId', $burnAmount)
         |  ]
       """.stripMargin

    val expr = Parser.parseContract(script).get.value
    val contract = compileContractFromExpr(expr, V4)
    ContractScript(V4, contract).explicitGet()
  }

  private val features = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Seq(
      BlockchainFeatures.SmartAccounts,
      BlockchainFeatures.SmartAssets,
      BlockchainFeatures.Ride4DApps,
      BlockchainFeatures.MultiPaymentInvokeScript,
    ).map(_.id -> 0).toMap
  )
}
