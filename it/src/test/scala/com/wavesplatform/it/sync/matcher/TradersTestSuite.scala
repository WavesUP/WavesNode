package com.wavesplatform.it.sync.matcher

import com.typesafe.config.Config
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.matcher.market.MatcherActor
import com.wavesplatform.matcher.model.MatcherModel.Price
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.util.Random

class TradersTestSuite extends MatcherSuiteBase {
  private val exTxFee                             = 300000
  private def orderVersion                        = (Random.nextInt(2) + 1).toByte
  override protected def nodeConfigs: Seq[Config] = Configs

  // Alice issues new asset
  private val aliceAsset =
    aliceNode.issue(aliceAcc.address, "AliceCoin", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, smartIssueFee, 2).id
  matcherNode.waitForTransaction(aliceAsset)

  // Wait for balance on Alice's account
  matcherNode.assertAssetBalance(aliceAcc.address, aliceAsset, someAssetAmount)
  matcherNode.assertAssetBalance(matcherAcc.address, aliceAsset, 0)
  matcherNode.assertAssetBalance(bobAcc.address, aliceAsset, 0)

  // Bob issues a new asset
  private val bobAssetQuantity = 10000
  private val bobNewAsset      = bobNode.issue(bobAcc.address, "BobCoin3", "Bob's asset", bobAssetQuantity, 0, reissuable = false, smartIssueFee, 2).id
  matcherNode.waitForTransaction(bobNewAsset)

  private val bobAssetId   = ByteStr.decodeBase58(bobNewAsset).get
  private val aliceAssetId = ByteStr.decodeBase58(aliceAsset).get

  private val bobWavesPair = AssetPair(
    amountAsset = Some(bobAssetId),
    priceAsset = None
  )

  private val twoAssetsPair =
    if (MatcherActor.compare(Some(bobAssetId.arr), Some(aliceAssetId.arr)) < 0)
      AssetPair(
        amountAsset = Some(aliceAssetId),
        priceAsset = Some(bobAssetId)
      )
    else
      AssetPair(
        amountAsset = Some(bobAssetId),
        priceAsset = Some(aliceAssetId)
      )

  matcherNode.assertAssetBalance(bobAcc.address, bobNewAsset, bobAssetQuantity)

  "AssetPair BOB/WAVES vs BOB/NULL" in {
    val trickyBobWavesPairWB58 = AssetPair(
      amountAsset = Some(bobAssetId),
      priceAsset = Some(ByteStr.decodeBase58("WAVES").get)
    )

    val trickyBobWavesPairWS = AssetPair(
      amountAsset = Some(bobAssetId),
      priceAsset = Some(ByteStr("WAVES".getBytes()))
    )

    val trickyBobOrderWB58 = matcherNode.prepareOrder(bobAcc, trickyBobWavesPairWB58, OrderType.BUY, 1, 10.waves * Order.PriceConstant)
    matcherNode.expectIncorrectOrderPlacement(trickyBobOrderWB58, 400, "OrderRejected")

    val trickyBobOrderWS = matcherNode.prepareOrder(bobAcc, trickyBobWavesPairWS, OrderType.BUY, 1, 10.waves * Order.PriceConstant)
    matcherNode.expectIncorrectOrderPlacement(trickyBobOrderWS, 400, "OrderRejected")

    val correctBobOrder   = matcherNode.prepareOrder(bobAcc, bobWavesPair, OrderType.BUY, 1, 10.waves * Order.PriceConstant)
    val correctBobOrderId = matcherNode.placeOrder(correctBobOrder).message.id
    matcherNode.waitOrderStatus(bobWavesPair, correctBobOrderId, "Accepted")

    matcherNode.orderBook(bobWavesPair).bids shouldNot be(empty)
    matcherNode.cancelOrder(bobAcc, bobWavesPair, correctBobOrderId)
    matcherNode.waitOrderStatus(bobWavesPair, correctBobOrderId, "Cancelled")
  }

  "Verifications of tricky ordering cases" - {
    "owner moves assets/waves to another account and order become an invalid" - {
      // Could not work sometimes because of NODE-546
      "order with assets" - {
        "moved assets, insufficient assets" in {
          val oldestOrderId = bobPlacesAssetOrder(4000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(4000, twoAssetsPair, bobNewAsset)

          val transferId = bobNode.transfer(bobAcc.address, aliceAcc.address, 5000, exTxFee, Some(bobNewAsset), None, 2).id
          matcherNode.waitForTransaction(transferId) // 5000 waves are rest

          withClue(s"The newest order '$newestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobWavesPair, newestOrderId, "Cancelled")
          }
          withClue(s"The oldest order '$oldestOrderId' is still active") {
            matcherNode.orderStatus(oldestOrderId, bobWavesPair).status shouldBe "Accepted"
          }

          // Cleanup
          matcherNode.cancelOrder(bobAcc, twoAssetsPair, oldestOrderId)
          matcherNode.waitOrderStatus(twoAssetsPair, oldestOrderId, "Cancelled")

          val transferBackId = aliceNode.transfer(aliceAcc.address, bobAcc.address, 5000, exTxFee, Some(bobNewAsset), None, 2).id
          matcherNode.waitForTransaction(transferBackId)
        }

        "leased waves, insufficient fee" in {
          val bobBalance    = matcherNode.accountBalances(bobAcc.address)._1
          val oldestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          // TransactionFee for leasing, matcherFee for one order
          val leaseAmount = bobBalance - exTxFee - matcherFee
          val leaseId     = bobNode.lease(bobAcc.address, aliceAcc.address, leaseAmount, exTxFee, 2).id
          matcherNode.waitForTransaction(leaseId)

          withClue(s"The newest order '$newestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobWavesPair, newestOrderId, "Cancelled")
          }
          withClue(s"The oldest order '$oldestOrderId' is still active") {
            matcherNode.orderStatus(oldestOrderId, bobWavesPair).status shouldBe "Accepted"
          }

          // Cleanup
          matcherNode.cancelOrder(bobAcc, twoAssetsPair, oldestOrderId)
          matcherNode.waitOrderStatus(twoAssetsPair, oldestOrderId, "Cancelled")

          val cancelLeaseId = bobNode.cancelLease(bobAcc.address, leaseId, exTxFee, 2).id
          matcherNode.waitForTransaction(cancelLeaseId)
        }

        "moved waves, insufficient fee" in {
          val bobBalance    = matcherNode.accountBalances(bobAcc.address)._1
          val oldestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          // TransactionFee for leasing, matcherFee for one order
          val transferAmount = bobBalance - exTxFee - matcherFee
          val transferId     = bobNode.transfer(bobAcc.address, aliceAcc.address, transferAmount, exTxFee, None, None, 2).id
          matcherNode.waitForTransaction(transferId)

          withClue(s"The newest order '$newestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobWavesPair, newestOrderId, "Cancelled")
          }
          withClue(s"The oldest order '$oldestOrderId' is still active") {
            matcherNode.orderStatus(oldestOrderId, bobWavesPair).status shouldBe "Accepted"
          }

          // Cleanup
          matcherNode.cancelOrder(bobAcc, twoAssetsPair, oldestOrderId)
          matcherNode.waitOrderStatus(twoAssetsPair, oldestOrderId, "Cancelled")

          val transferBackId = aliceNode.transfer(aliceAcc.address, bobAcc.address, transferAmount, exTxFee, None, None, 2).id
          matcherNode.waitForTransaction(transferBackId)
        }
      }

      "order with waves" - {
        "leased waves, insufficient fee for one ExchangeTransaction" in {
          // Amount of waves in order is smaller than fee
          val bobBalance = matcherNode.accountBalances(bobAcc.address)._1

          val oldestOrderId = bobPlacesWaveOrder(bobWavesPair, 1, 10.waves * Order.PriceConstant)
          val newestOrderId = bobPlacesWaveOrder(bobWavesPair, 1, 10.waves * Order.PriceConstant)

          //      waitForOrderStatus(matcherNode, bobAssetIdRaw, id, "Accepted")
          val leaseAmount = bobBalance - exTxFee - 10.waves - matcherFee
          val leaseId     = bobNode.lease(bobAcc.address, aliceAcc.address, leaseAmount, exTxFee, 2).id
          matcherNode.waitForTransaction(leaseId)

          withClue(s"The newest order '$newestOrderId' is Cancelled") {
            matcherNode.waitOrderStatus(bobWavesPair, newestOrderId, "Cancelled")
          }
          withClue(s"The oldest order '$oldestOrderId' is still active") {
            matcherNode.orderStatus(oldestOrderId, bobWavesPair).status shouldBe "Accepted"
          }

          // Cleanup
          matcherNode.cancelOrder(bobAcc, bobWavesPair, oldestOrderId)
          matcherNode.waitOrderStatus(twoAssetsPair, oldestOrderId, "Cancelled")

          val cancelLeaseId = bobNode.cancelLease(bobAcc.address, leaseId, exTxFee, 2).id
          matcherNode.waitForTransaction(cancelLeaseId)
        }

        "leased waves, insufficient waves" in {
          val bobBalance = matcherNode.accountBalances(bobAcc.address)._1
          val price      = 1.waves
          val order2     = bobPlacesWaveOrder(bobWavesPair, 1, price * Order.PriceConstant)

          val leaseAmount = bobBalance - exTxFee - price / 2
          val leaseId     = bobNode.lease(bobAcc.address, aliceAcc.address, leaseAmount, exTxFee, 2).id
          matcherNode.waitForTransaction(leaseId)

          withClue(s"The order '$order2' was cancelled") {
            matcherNode.waitOrderStatus(bobWavesPair, order2, "Cancelled")
          }

          // Cleanup
          val cancelLeaseId = bobNode.cancelLease(bobAcc.address, leaseId, exTxFee, 2).id
          matcherNode.waitForTransaction(cancelLeaseId)
        }

        "moved waves, insufficient fee" in {
          // Amount of waves in order is smaller than fee
          val bobBalance = matcherNode.accountBalances(bobAcc.address)._1
          val price      = exTxFee / 2
          val order3     = bobPlacesWaveOrder(bobWavesPair, 1, price * Order.PriceConstant)

          val transferAmount = bobBalance - exTxFee - price
          val txId           = bobNode.transfer(bobAcc.address, aliceAcc.address, transferAmount, exTxFee, None, None, 2).id
          matcherNode.waitForTransaction(txId)

          withClue(s"The order '$order3' was cancelled") {
            matcherNode.waitOrderStatus(bobWavesPair, order3, "Cancelled")
          }

          // Cleanup
          val transferBackId = aliceNode.transfer(aliceAcc.address, bobAcc.address, transferAmount, exTxFee, None, None, 2).id
          matcherNode.waitForTransaction(transferBackId)
        }

      }
    }
  }

  def bobPlacesWaveOrder(assetPair: AssetPair, amount: Long, price: Price): String = {
    val bobOrder = matcherNode.prepareOrder(bobAcc, assetPair, OrderType.BUY, amount, price)
    val order    = matcherNode.placeOrder(bobOrder).message.id
    matcherNode.waitOrderStatus(assetPair, order, "Accepted")
    order
  }

  def bobPlacesAssetOrder(bobCoinAmount: Int, twoAssetsPair: AssetPair, assetId: String): String = {
    val decodedAsset = ByteStr.decodeBase58(assetId).get
    val bobOrder = if (twoAssetsPair.amountAsset.contains(decodedAsset)) {
      matcherNode.prepareOrder(bobAcc, twoAssetsPair, OrderType.SELL, bobCoinAmount, 1 * Order.PriceConstant, exTxFee, orderVersion)
    } else {
      matcherNode.prepareOrder(bobAcc, twoAssetsPair, OrderType.BUY, 1, bobCoinAmount * Order.PriceConstant, exTxFee, orderVersion)
    }
    val order = matcherNode.placeOrder(bobOrder)
    matcherNode.waitOrderStatus(twoAssetsPair, order.message.id, "Accepted")
    order.message.id
  }

}
