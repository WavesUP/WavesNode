package com.wavesplatform.it.sync.transactions

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.it.NTPTime
import com.wavesplatform.it.sync.someAssetAmount
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.it.sync._
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.transaction.assets.IssueTransactionV1
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransactionV1, ExchangeTransactionV2, Order, OrderV1}
import io.grpc.StatusRuntimeException
import org.scalatest.{Tag}

class ExchangeGrpcSuite extends BaseTransactionSuite with NTPTime {
  val exchAsset: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = sender.privateKey,
      name = "myasset".getBytes("UTF-8"),
      description = "my asset description".getBytes("UTF-8"),
      quantity = someAssetAmount,
      decimals = 2,
      reissuable = true,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  private val seller  = pkByAddress(firstAddress)
  private val buyer   = pkByAddress(secondAddress)
  private val matcher = pkByAddress(thirdAddress)

  val transactionV1versions = (1: Byte, 1: Byte, 1: Byte) // in ExchangeTransactionV1 only orders V1 are supported
  val transactionV2versions = for {
    o1ver <- 1 to 3
    o2ver <- 1 to 3
  } yield (o1ver.toByte, o2ver.toByte, 2.toByte)
  val versions = transactionV1versions +: transactionV2versions

  test("cannot exchange non-issued assets", new Tag("grpc_and_rest")) {
    for ((o1ver, o2ver, tver) <- versions) {

      val assetId = exchAsset.id().base58

      val ts                  = ntpTime.correctedTime()
      val expirationTimestamp = ts + Order.MaxLiveTime
      val buyPrice            = 2 * Order.PriceConstant
      val sellPrice           = 2 * Order.PriceConstant
      val buyAmount           = 1
      val sellAmount          = 1
      val pair = AssetPair.createAssetPair("WAVES", assetId).get
      val buy  = Order.buy(buyer, matcher, pair, buyAmount, buyPrice, ts, expirationTimestamp, matcherFee, o1ver)
      val sell = Order.sell(seller, matcher, pair, sellAmount, sellPrice, ts, expirationTimestamp, matcherFee, o2ver)
      val amount = 1

      val buyMatcherFee = (BigInt(matcherFee) * amount / buy.amount).toLong
      val sellMatcherFee = (BigInt(matcherFee) * amount / sell.amount).toLong
      if (tver != 1) {

        assertBadRequestAndMessage(sender.exchange(matcher, buy, sell, amount, sellPrice, buyMatcherFee, sellMatcherFee, matcherFee, ts, version = 2, waitForTx = true),"Assets should be issued before they can be traded")


      } else {

        assertBadRequestAndMessage(sender.exchange(matcher, buy, sell, amount, sellPrice, buyMatcherFee, sellMatcherFee, matcherFee, ts, version = 1, waitForTx = true),"Assets should be issued before they can be traded")

      }
    }
  }
}
