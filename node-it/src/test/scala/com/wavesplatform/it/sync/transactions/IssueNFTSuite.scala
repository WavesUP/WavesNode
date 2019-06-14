package com.wavesplatform.it.sync.transactions

import com.typesafe.config.Config
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils._
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.it.{Node, NodeConfigs}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.IssueTransactionV2
import com.wavesplatform.transaction.transfer.TransferTransactionV1
import org.scalatest.prop.TableDrivenPropertyChecks

class IssueNFTSuite extends BaseTransactionSuite with TableDrivenPropertyChecks {

  val firstNode: Node  = nodes.head
  val secondNode: Node = nodes.last

  val secondNodeIssuer = KeyPair("second_node_issuer".getBytes())
  val firstNodeIssuer  = KeyPair("first_node_issuer".getBytes())

  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(1)
      .withSpecial(_.raw(s"""
           |waves.blockchain.custom.functionality.pre-activated-features = {
           |          2 = 0
           |          3 = 0
           |          4 = 0
           |          5 = 0
           |          6 = 0
           |          7 = 0
           |          9 = 0
           |          10 = 0
           |          11 = 0
           |          12 = 0
           |          13 = 0
           |}
         """.stripMargin))
      .buildNonConflicting()

  test("Can't issue NFT before activation") {
    val assetName        = "NFTAsset"
    val assetDescription = "my asset description"

    firstNode.transfer(
      firstNode.privateKey.address,
      firstNodeIssuer.address,
      10.waves,
      0.001.waves,
      waitForTx = true
    )

    assertBadRequest(
      firstNode.issue(firstAddress, assetName, assetDescription, 1, 0, reissuable = false, 1.waves / 1000, waitForTx = true)
    )
  }

  test("Able to issue NFT token with reduced fee") {
    val assetName        = "NFTAsset"
    val assetDescription = "my asset description"

    val nftIssueTxId = secondNode.issue(secondNode.address,
                                         assetName,
                                         assetDescription,
                                         quantity = 1,
                                         decimals = 0,
                                         reissuable = false,
                                         fee = 0.001.waves,
                                         script = None,
                                         waitForTx = true).id

    secondNode.assertAssetBalance(secondNode.address, nftIssueTxId, 1L)
  }

  test("Can't issue reissuable NFT") {
    val assetName        = "NFTAsset"
    val assetDescription = "my asset description"

    assertBadRequestAndResponse(secondNode.issue(secondNode.address,
      assetName,
      assetDescription,
      quantity = 1,
      decimals = 0,
      reissuable = true,
      fee = 0.001.waves,
      script = None,
      waitForTx = true), "does not exceed minimal value")
  }

  test("Can't issue NFT with quantity > 1") {
    val assetName        = "NFTAsset"
    val assetDescription = "my asset description"

    assertBadRequestAndResponse(secondNode.issue(secondNode.address,
      assetName,
      assetDescription,
      quantity = 2,
      decimals = 0,
      reissuable = false,
      fee = 0.001.waves,
      script = None,
      waitForTx = true), "does not exceed minimal value")
  }

  test("Can't issue token with reduced fee if decimals > 0") {
    val assetName        = "NFTAsset"
    val assetDescription = "my asset description"

    assertBadRequestAndResponse(secondNode.issue(secondNode.address,
      assetName,
      assetDescription,
      quantity = 1,
      decimals = 1,
      reissuable = false,
      fee = 0.001.waves,
      script = None,
      waitForTx = true), "does not exceed minimal value")
  }
}
