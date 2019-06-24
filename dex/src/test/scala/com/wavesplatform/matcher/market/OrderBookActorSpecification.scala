package com.wavesplatform.matcher.market

import java.util.concurrent.ConcurrentHashMap

import akka.actor.{ActorRef, Props}
import akka.persistence.serialization.Snapshot
import akka.testkit.{ImplicitSender, TestProbe}
import cats.data.NonEmptyList
import com.wavesplatform.NTPTime
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.matcher.api.AlreadyProcessed
import com.wavesplatform.matcher.fixtures.RestartableActor
import com.wavesplatform.matcher.fixtures.RestartableActor.RestartActor
import com.wavesplatform.matcher.market.MatcherActor.SaveSnapshot
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.matcher.model.OrderBook.TickSize
import com.wavesplatform.matcher.model._
import com.wavesplatform.matcher.queue.QueueEvent.Canceled
import com.wavesplatform.matcher.settings.MatchingRules
import com.wavesplatform.matcher.{MatcherTestData, SnapshotUtils}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.OrderOps._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.wavesplatform.utils.EmptyBlockchain
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.concurrent.Eventually

import scala.concurrent.duration._

class OrderBookActorSpecification
    extends MatcherSpec("OrderBookActor")
    with NTPTime
    with ImplicitSender
    with MatcherTestData
    with PathMockFactory
    with Eventually {

  private val txFactory = new ExchangeTransactionCreator(EmptyBlockchain, MatcherAccount, matcherSettings).createTransaction _
  private val obc       = new ConcurrentHashMap[AssetPair, OrderBook.AggregatedSnapshot]
  private val md        = new ConcurrentHashMap[AssetPair, MarketStatus]

  private val wctAsset = IssuedAsset(ByteStr("WCT".getBytes))
  private val ethAsset = IssuedAsset(ByteStr("ETH".getBytes))

  private def update(ap: AssetPair)(snapshot: OrderBook.AggregatedSnapshot): Unit = obc.put(ap, snapshot)

  private def obcTest(f: (AssetPair, ActorRef, TestProbe) => Unit): Unit = obcTestWithPrepare(_ => ()) { (pair, actor, probe) =>
    probe.expectMsg(OrderBookRecovered(pair, None))
    f(pair, actor, probe)
  }

  private def obcTestWithTickSize(tickSize: TickSize)(f: (AssetPair, ActorRef, TestProbe) => Unit): Unit =
    obcTestWithPrepare(_ => (), NonEmptyList(MatchingRules(0L, tickSize), List.empty)) { (pair, actor, probe) =>
      probe.expectMsg(OrderBookRecovered(pair, None))
      f(pair, actor, probe)
    }

  private def obcTestWithMatchingRules(matchingRules: NonEmptyList[MatchingRules])(f: (AssetPair, ActorRef, TestProbe) => Unit): Unit =
    obcTestWithPrepare(_ => (), matchingRules) { (pair, actor, probe) =>
      probe.expectMsg(OrderBookRecovered(pair, None))
      f(pair, actor, probe)
    }

  private def obcTestWithPrepare(prepare: AssetPair => Unit, matchingRules: NonEmptyList[MatchingRules] = MatchingRules.DefaultNel)(
      f: (AssetPair, ActorRef, TestProbe) => Unit): Unit = {

    obc.clear()
    md.clear()

    val tp   = TestProbe()
    val pair = AssetPair(wctAsset, Waves)

    prepare(pair)

    val orderBookActor =
      system.actorOf(
        Props(
          new OrderBookActor(
            owner = tp.ref,
            addressActor = tp.ref,
            assetPair = pair,
            updateSnapshot = update(pair),
            updateMarketStatus = p => Option(md.get(p)),
            createTransaction = txFactory,
            time = ntpTime,
            matchingRules = matchingRules
          ) with RestartableActor
        )
      )

    f(pair, orderBookActor, tp)
  }

  "OrderBookActor" should {
    "recover from snapshot - 1" in obcTestWithPrepare(_ => ()) { (pair, _, tp) =>
      tp.expectMsg(OrderBookRecovered(pair, None))
    }

    "recover from snapshot - 2" in obcTestWithPrepare { p =>
      SnapshotUtils.provideSnapshot(
        OrderBookActor.name(p),
        Snapshot(OrderBookActor.Snapshot(Some(50), OrderBook.empty.snapshot))
      )
    } { (pair, _, tp) =>
      tp.expectMsg(OrderBookRecovered(pair, Some(50)))
    }

    "place buy and sell order to the order book and preserve it after restart" in obcTest { (pair, orderBook, tp) =>
      val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
      val ord2 = sell(pair, 15 * Order.PriceConstant, 150)

      orderBook ! wrapLimitOrder(ord1)
      orderBook ! wrapLimitOrder(ord2)
      tp.receiveN(2)

      orderBook ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdated]
      orderBook ! RestartActor

      tp.receiveN(2) shouldEqual Seq(ord1, ord2).map(o => OrderAdded(LimitOrder(o), o.timestamp))
    }

    "execute partial market orders and preserve remaining after restart" in obcTest { (pair, actor, tp) =>
      val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
      val ord2 = sell(pair, 15 * Order.PriceConstant, 100)

      actor ! wrapLimitOrder(ord1)
      actor ! wrapLimitOrder(ord2)

      tp.receiveN(3)

      actor ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdated]
      actor ! RestartActor

      tp.expectMsg(
        OrderAdded(SellLimitOrder(
                     ord2.amount - ord1.amount,
                     ord2.matcherFee - AcceptedOrder.partialFee(ord2.matcherFee, ord2.amount, ord1.amount),
                     ord2
                   ),
                   ord2.timestamp)
      )
    }

    "execute one order fully and other partially and restore after restart" in obcTest { (pair, actor, tp) =>
      val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
      val ord2 = buy(pair, 5 * Order.PriceConstant, 100)
      val ord3 = sell(pair, 12 * Order.PriceConstant, 100)

      actor ! wrapLimitOrder(ord1)
      actor ! wrapLimitOrder(ord2)
      actor ! wrapLimitOrder(ord3)
      tp.receiveN(4)

      actor ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdated]
      actor ! RestartActor

      val restAmount = ord1.amount + ord2.amount - ord3.amount
      tp.expectMsg(
        OrderAdded(BuyLimitOrder(
                     restAmount,
                     ord2.matcherFee - AcceptedOrder.partialFee(ord2.matcherFee, ord2.amount, ord2.amount - restAmount),
                     ord2
                   ),
                   ord2.timestamp)
      )
    }

    "match multiple best orders at once and restore after restart" in obcTest { (pair, actor, tp) =>
      val ord1 = sell(pair, 10 * Order.PriceConstant, 100)
      val ord2 = sell(pair, 5 * Order.PriceConstant, 100)
      val ord3 = sell(pair, 5 * Order.PriceConstant, 90)
      val ord4 = buy(pair, 19 * Order.PriceConstant, 100)

      actor ! wrapLimitOrder(ord1)
      actor ! wrapLimitOrder(ord2)
      actor ! wrapLimitOrder(ord3)
      actor ! wrapLimitOrder(ord4)
      tp.receiveN(6)

      actor ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdated]
      actor ! RestartActor

      val restAmount = ord1.amount + ord2.amount + ord3.amount - ord4.amount
      tp.expectMsg(
        OrderAdded(
          SellLimitOrder(
            restAmount,
            ord2.matcherFee - AcceptedOrder.partialFee(ord2.matcherFee, ord2.amount, ord2.amount - restAmount),
            ord2
          ),
          ord2.timestamp
        ))
    }

    "place orders and restart without waiting for response" in obcTest { (pair, orderBook, tp) =>
      val ord1 = sell(pair, 10 * Order.PriceConstant, 100)
      val ts   = System.currentTimeMillis()

      (1 to 100) foreach { i =>
        orderBook ! wrapLimitOrder(ord1.updateTimestamp(ts + i))
      }

      within(10.seconds) {
        tp.receiveN(100)
      }

      orderBook ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdated]
      orderBook ! RestartActor

      within(10.seconds) {
        tp.receiveN(100)
      }
    }

    "ignore outdated requests" in obcTest { (pair, actor, tp) =>
      (1 to 10).foreach { i =>
        actor ! wrapLimitOrder(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      (1 to 10).foreach { i =>
        actor ! wrapLimitOrder(i, buy(pair, 100000000, 0.00041))
      }
      all(receiveN(10)) shouldBe AlreadyProcessed
    }

    "respond on SaveSnapshotCommand" in obcTest { (pair, actor, tp) =>
      (1 to 10).foreach { i =>
        actor ! wrapLimitOrder(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(10)
      tp.expectMsg(OrderBookSnapshotUpdated(pair, 10))

      (11 to 20).foreach { i =>
        actor ! wrapLimitOrder(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(20)
      tp.expectMsg(OrderBookSnapshotUpdated(pair, 20))
    }

    "don't do a snapshot if there is no changes" in obcTest { (pair, actor, tp) =>
      (1 to 10).foreach { i =>
        actor ! wrapLimitOrder(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(10)
      actor ! SaveSnapshot(10)
      tp.expectMsgType[OrderBookSnapshotUpdated]
      tp.expectNoMessage(200.millis)
    }

    "restore its state at start" in obcTest { (pair, actor, tp) =>
      (1 to 10).foreach { i =>
        actor ! wrapLimitOrder(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(10)
      tp.expectMsgType[OrderBookSnapshotUpdated]
    }

    "cancel order in merge small prices mode" in obcTestWithTickSize(TickSize.Enabled(100)) { (pair, orderBook, tp) =>
      val buyOrder = buy(pair, 100000000, 0.0000041)

      orderBook ! wrapLimitOrder(1, buyOrder)
      tp.expectMsgType[OrderAdded]

      orderBook ! wrapEvent(2, Canceled(buyOrder.assetPair, buyOrder.id()))
      tp.expectMsgType[OrderCanceled]
    }

    val switchRulesTest = NonEmptyList(
      MatchingRules.Default,
      List(
        MatchingRules(4, OrderBook.TickSize.Enabled(100)),
        MatchingRules(10, OrderBook.TickSize.Enabled(300))
      )
    )

    "rules are switched" in obcTestWithMatchingRules(switchRulesTest) { (pair, orderBook, tp) =>
      val buyOrder = buy(pair, 100000000, 0.0000041)
      (0 to 17).foreach { i =>
        orderBook ! wrapLimitOrder(i, buyOrder)
        tp.expectMsgType[OrderAdded]
      }

      eventually {
        val bids = obc.get(pair).bids
        bids.size shouldBe 3

        val level41 = bids.head
        level41.price shouldBe buyOrder.price
        level41.amount shouldBe buyOrder.amount * 4

        val level40 = bids(1)
        level40.price shouldBe (0.000004 * Order.PriceConstant)
        level40.amount shouldBe buyOrder.amount * 6

        val level30 = bids(2)
        level30.price shouldBe (0.000003 * Order.PriceConstant)
        level30.amount shouldBe buyOrder.amount * 8
      }
    }

    val disableRulesTest = NonEmptyList(
      MatchingRules(0, OrderBook.TickSize.Enabled(100)),
      List(
        MatchingRules(3, OrderBook.TickSize.Disabled)
      )
    )

    "rules can be disabled" in obcTestWithMatchingRules(disableRulesTest) { (pair, orderBook, tp) =>
      val buyOrder = buy(pair, 100000000, 0.0000041)
      (0 to 10).foreach { i =>
        orderBook ! wrapLimitOrder(i, buyOrder)
        tp.expectMsgType[OrderAdded]
      }

      eventually {
        val bids = obc.get(pair).bids
        bids.size shouldBe 2

        val level41 = bids.head
        level41.price shouldBe buyOrder.price
        level41.amount shouldBe buyOrder.amount * 8

        val level40 = bids(1)
        level40.price shouldBe (0.000004 * Order.PriceConstant)
        level40.amount shouldBe buyOrder.amount * 3
      }
    }

    val matchingRulesForCancelTest = NonEmptyList(
      MatchingRules.Default,
      List(
        MatchingRules.Default,
        MatchingRules(1, OrderBook.TickSize.Enabled(100))
      )
    )

    "correctly cancel order when rules are switched" in obcTestWithMatchingRules(matchingRulesForCancelTest) { (pair, orderBook, tp) =>
      val buyOrder1, buyOrder2 = buy(pair, 100000000, 0.0000041)

      orderBook ! wrapLimitOrder(0, buyOrder1) // order book places order to the price level 41
      tp.expectMsgType[OrderAdded]

      orderBook ! wrapLimitOrder(1, buyOrder2) // now order book places the same order to the price level 40
      tp.expectMsgType[OrderAdded]

      eventually {
        val bids = obc.get(pair).bids
        bids.size shouldBe 2
        bids.head.price shouldBe buyOrder1.price
        bids.last.price shouldBe 0.0000040 * Order.PriceConstant
      }

      orderBook ! wrapEvent(2, Canceled(buyOrder1.assetPair, buyOrder1.id())) // order book is looking for the price level of buyOrder1 correctly (41 but not 40)
      tp.expectMsgType[OrderCanceled]

      eventually {
        val bids = obc.get(pair).bids
        bids.size shouldBe 1
        bids.head.price shouldBe 0.000004 * Order.PriceConstant
      }
    }

    "correctly handle big market orders" in {

      def bigMarketOrderTest(marketOrderType: OrderType, feeAsset: Asset): Unit = obcTest { (wctWavesPair, orderBook, tp) =>
        val counterOrder1Amount = toNormalized(12) // will be executed fully
        val counterOrder2Amount = toNormalized(5)  // will be executed fully
        val counterOrder3Amount = toNormalized(3)  // will be executed partially

        val marketOrderAmount = toNormalized(18)

        val (counterOrder1, counterOrder2, counterOrder3, marketOrder) = marketOrderType match {
          case OrderType.BUY =>
            val buyOrder       = buy(wctWavesPair, amount = marketOrderAmount, price = 110, matcherFee = smallFee, version = 3, feeAsset = feeAsset)
            val marketBuyOrder = MarketOrder(buyOrder, availableForSpending = getSpentAmountWithFee(buyOrder))
            (
              sell(wctWavesPair, amount = counterOrder1Amount, price = 105),
              sell(wctWavesPair, amount = counterOrder2Amount, price = 110),
              sell(wctWavesPair, amount = counterOrder3Amount, price = 110),
              marketBuyOrder
            )
          case OrderType.SELL =>
            val sellOrder       = sell(wctWavesPair, amount = marketOrderAmount, price = 95, matcherFee = smallFee, version = 3, feeAsset = feeAsset)
            val marketSellOrder = MarketOrder(sellOrder, availableForSpending = getSpentAmountWithFee(sellOrder))
            (
              buy(wctWavesPair, amount = counterOrder1Amount, price = 110),
              buy(wctWavesPair, amount = counterOrder2Amount, price = 105),
              buy(wctWavesPair, amount = counterOrder3Amount, price = 105),
              marketSellOrder
            )
        }

        Seq(counterOrder1, counterOrder2, counterOrder3).foreach { o =>
          orderBook ! wrapLimitOrder(o)
          tp.expectMsgType[OrderAdded]
        }

        orderBook ! wrapMarketOrder(marketOrder)

        val oe1 = tp.expectMsgType[OrderExecuted]
        oe1.submitted shouldBe marketOrder
        oe1.counter shouldBe LimitOrder(counterOrder1)
        oe1.executedAmount shouldBe counterOrder1.amount

        val oe2                   = tp.expectMsgType[OrderExecuted]
        val marketOrderRemaining1 = oe1.submittedMarketRemaining(marketOrder)
        oe2.submitted shouldBe marketOrderRemaining1
        oe2.counter shouldBe LimitOrder(counterOrder2)
        oe2.executedAmount shouldBe counterOrder2.amount

        val oe3                   = tp.expectMsgType[OrderExecuted]
        val marketOrderRemaining2 = oe2.submittedMarketRemaining(marketOrderRemaining1)
        oe3.submitted shouldBe marketOrderRemaining2
        oe3.counter shouldBe LimitOrder(counterOrder3)
        oe3.executedAmount shouldBe toNormalized(1)

        tp.receiveN(0)

        eventually {
          obc.get(wctWavesPair).getCounterSideFor(marketOrder).map(_.amount).sum shouldBe toNormalized(2)
          obc.get(wctWavesPair).getSideFor(marketOrder) shouldBe empty
        }
      }

      bigMarketOrderTest(OrderType.SELL, feeAsset = Waves)    // fee in received asset
      bigMarketOrderTest(OrderType.SELL, feeAsset = ethAsset) // fee in third asset
      bigMarketOrderTest(OrderType.SELL, feeAsset = wctAsset) // fee in spent asset

      bigMarketOrderTest(OrderType.BUY, feeAsset = wctAsset) // fee in received asset
      bigMarketOrderTest(OrderType.BUY, feeAsset = ethAsset) // fee in third asset
      bigMarketOrderTest(OrderType.BUY, feeAsset = Waves)    // fee in spent asset
    }

    "cancel market orders because of the stop conditions (no counter orders or partially filled)" in {

      def noCountersOrPartiallyFilledTest(marketOrderType: OrderType, feeAsset: Asset): Unit = obcTest { (wctWavesPair, orderBook, tp) =>
        val marketOrderAmount  = toNormalized(10)
        val counterOrderAmount = toNormalized(4)

        val (counterOrder, marketOrder) = marketOrderType match {
          case OrderType.SELL =>
            val sellOrder       = sell(wctWavesPair, amount = marketOrderAmount, price = 90, matcherFee = smallFee, version = 3, feeAsset = feeAsset)
            val marketSellOrder = MarketOrder(sellOrder, availableForSpending = getSpentAmountWithFee(sellOrder))
            (
              buy(wctWavesPair, amount = counterOrderAmount, price = 100, matcherFee = smallFee, version = 3),
              marketSellOrder
            )
          case OrderType.BUY =>
            val buyOrder       = buy(wctWavesPair, amount = marketOrderAmount, price = 100, matcherFee = smallFee, version = 3, feeAsset = feeAsset)
            val marketBuyOrder = MarketOrder(buyOrder, availableForSpending = getSpentAmountWithFee(buyOrder))
            (
              sell(wctWavesPair, amount = counterOrderAmount, price = 90, matcherFee = smallFee, version = 3),
              marketBuyOrder
            )
        }

        withClue("Stop condition - no counter orders:") {
          orderBook ! wrapMarketOrder(marketOrder)
          val oc = tp.expectMsgType[OrderCanceled]

          oc.acceptedOrder shouldBe marketOrder
          oc.isSystemCancel shouldBe true
          obc.get(wctWavesPair).asks shouldBe empty

          tp.receiveN(0)
        }

        withClue("Stop condition - market order partially filled:") {
          orderBook ! wrapLimitOrder(counterOrder)
          tp.expectMsgType[OrderAdded]

          orderBook ! wrapMarketOrder(marketOrder)

          val oe = tp.expectMsgType[OrderExecuted]
          oe.submitted shouldBe marketOrder
          oe.counter shouldBe LimitOrder(counterOrder)
          oe.executedAmount shouldBe counterOrder.amount

          val oc2 = tp.expectMsgType[OrderCanceled]

          oc2.acceptedOrder shouldBe oe.submittedMarketRemaining(marketOrder)
          oc2.isSystemCancel shouldBe true

          eventually {
            obc.get(wctWavesPair).asks shouldBe empty
            obc.get(wctWavesPair).bids shouldBe empty
          }

          tp.receiveN(0)
        }
      }

      noCountersOrPartiallyFilledTest(marketOrderType = OrderType.SELL, feeAsset = Waves)    // fee in received asset
      noCountersOrPartiallyFilledTest(marketOrderType = OrderType.SELL, feeAsset = ethAsset) // fee in third asset
      noCountersOrPartiallyFilledTest(marketOrderType = OrderType.SELL, feeAsset = wctAsset) // fee in spent asset

      noCountersOrPartiallyFilledTest(marketOrderType = OrderType.BUY, feeAsset = wctAsset) // fee in received asset
      noCountersOrPartiallyFilledTest(marketOrderType = OrderType.BUY, feeAsset = ethAsset) // fee in third asset
      noCountersOrPartiallyFilledTest(marketOrderType = OrderType.BUY, feeAsset = Waves)    // fee in spent asset
    }

    "cancel market orders because of the stop conditions (available balance for spending is exhausted)" in {

      def afsIsNotEnoughTest(marketOrderType: OrderType, feeAsset: Asset, isFeeConsideredInExecutionAmount: Boolean): Unit =
        obcTest { (wctWavesPair, orderBook, tp) =>
          val moAmount             = toNormalized(4)
          val availableForSpending = moAmount / 2

          val (counterOrder, marketOrder) = marketOrderType match {
            case OrderType.SELL =>
              val buyOrder = buy(wctWavesPair, amount = moAmount, price = 100, matcherFee = smallFee, version = 3)
              val marketSellOrder =
                MarketOrder(
                  buyOrder.updateType(OrderType.SELL).updateMatcherFeeAssetId(feeAsset),
                  availableForSpending = availableForSpending
                )
              buyOrder -> marketSellOrder
            case OrderType.BUY =>
              val sellOrder = sell(wctWavesPair, amount = moAmount, price = 90, matcherFee = smallFee, version = 3)
              val marketBuyOrder =
                MarketOrder(
                  sellOrder.updateType(OrderType.BUY).updateMatcherFeeAssetId(feeAsset),
                  availableForSpending = availableForSpending
                )
              sellOrder -> marketBuyOrder
          }

          orderBook ! wrapLimitOrder(counterOrder)
          tp.expectMsgType[OrderAdded]

          orderBook ! wrapMarketOrder(marketOrder)

          val oe = tp.expectMsgType[OrderExecuted]
          oe.submitted shouldBe marketOrder
          oe.counter shouldBe LimitOrder(counterOrder)

          (marketOrderType, isFeeConsideredInExecutionAmount) match {
            case (OrderType.SELL, true)  => oe.executedAmount + oe.submittedExecutedFee shouldBe marketOrder.availableForSpending
            case (OrderType.SELL, false) => oe.executedAmount shouldBe marketOrder.availableForSpending

            case (OrderType.BUY, true) =>
              oe.executedAmountOfPriceAsset + oe.submittedExecutedFee should be <= marketOrder.availableForSpending
              MatcherModel.getCost(oe.executedAmount + 1, marketOrder.price) + oe.submittedExecutedFee should be > marketOrder.availableForSpending

            case (OrderType.BUY, false) =>
              oe.executedAmountOfPriceAsset should be <= marketOrder.availableForSpending
              MatcherModel.getCost(oe.executedAmount + 1, marketOrder.price) should be > marketOrder.availableForSpending
          }

          val oc = tp.expectMsgType[OrderCanceled]

          oc.acceptedOrder shouldBe oe.submittedMarketRemaining(marketOrder)
          oc.isSystemCancel shouldBe true

          tp.receiveN(0)

          eventually {
            obc.get(wctWavesPair).getSideFor(marketOrder) shouldBe empty
            obc.get(wctWavesPair).getCounterSideFor(marketOrder).map(_.amount).sum shouldBe counterOrder.amount - oe.executedAmount
          }
        }

      afsIsNotEnoughTest(marketOrderType = OrderType.SELL, feeAsset = Waves, isFeeConsideredInExecutionAmount = false)    // fee in received asset
      afsIsNotEnoughTest(marketOrderType = OrderType.SELL, feeAsset = ethAsset, isFeeConsideredInExecutionAmount = false) // fee in third asset
      afsIsNotEnoughTest(marketOrderType = OrderType.SELL, feeAsset = wctAsset, isFeeConsideredInExecutionAmount = true)  // fee in spent asset

      afsIsNotEnoughTest(marketOrderType = OrderType.BUY, feeAsset = wctAsset, isFeeConsideredInExecutionAmount = false) // fee in received asset
      afsIsNotEnoughTest(marketOrderType = OrderType.BUY, feeAsset = ethAsset, isFeeConsideredInExecutionAmount = false) // fee in third asset
      afsIsNotEnoughTest(marketOrderType = OrderType.BUY, feeAsset = Waves, isFeeConsideredInExecutionAmount = true)     // fee in spent asset
    }
  }
}
