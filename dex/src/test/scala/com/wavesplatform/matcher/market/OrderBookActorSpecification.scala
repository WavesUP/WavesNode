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
import com.wavesplatform.matcher.model.Events.{OrderAdded, OrderCanceled}
import com.wavesplatform.matcher.model.OrderBook.TickSize
import com.wavesplatform.matcher.model._
import com.wavesplatform.matcher.queue.QueueEvent.Canceled
import com.wavesplatform.matcher.settings.MatchingRules
import com.wavesplatform.matcher.{MatcherTestData, SnapshotUtils}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.OrderOps._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils.EmptyBlockchain
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.concurrent.Eventually

import scala.concurrent.duration._
import scala.util.Random

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
    val b = ByteStr(new Array[Byte](32))
    Random.nextBytes(b.arr)

    val tp   = TestProbe()
    val pair = AssetPair(IssuedAsset(b), Waves)
    prepare(pair)
    val actor = system.actorOf(
      Props(
        new OrderBookActor(
          tp.ref,
          tp.ref,
          pair,
          update(pair),
          p => Option(md.get(p)),
          txFactory,
          ntpTime,
          matchingRules
        ) with RestartableActor))

    f(pair, actor, tp)
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

      orderBook ! wrap(ord1)
      orderBook ! wrap(ord2)
      tp.receiveN(2)

      orderBook ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdated]
      orderBook ! RestartActor

      tp.receiveN(2) shouldEqual Seq(ord1, ord2).map(o => OrderAdded(LimitOrder(o), o.timestamp))
    }

    "execute partial market orders and preserve remaining after restart" in obcTest { (pair, actor, tp) =>
      val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
      val ord2 = sell(pair, 15 * Order.PriceConstant, 100)

      actor ! wrap(ord1)
      actor ! wrap(ord2)

      tp.receiveN(3)

      actor ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdated]
      actor ! RestartActor

      tp.expectMsg(
        OrderAdded(SellLimitOrder(
                     ord2.amount - ord1.amount,
                     ord2.matcherFee - LimitOrder.partialFee(ord2.matcherFee, ord2.amount, ord1.amount),
                     ord2
                   ),
                   ord2.timestamp)
      )
    }

    "execute one order fully and other partially and restore after restart" in obcTest { (pair, actor, tp) =>
      val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
      val ord2 = buy(pair, 5 * Order.PriceConstant, 100)
      val ord3 = sell(pair, 12 * Order.PriceConstant, 100)

      actor ! wrap(ord1)
      actor ! wrap(ord2)
      actor ! wrap(ord3)
      tp.receiveN(4)

      actor ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdated]
      actor ! RestartActor

      val restAmount = ord1.amount + ord2.amount - ord3.amount
      tp.expectMsg(
        OrderAdded(BuyLimitOrder(
                     restAmount,
                     ord2.matcherFee - LimitOrder.partialFee(ord2.matcherFee, ord2.amount, ord2.amount - restAmount),
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

      actor ! wrap(ord1)
      actor ! wrap(ord2)
      actor ! wrap(ord3)
      actor ! wrap(ord4)
      tp.receiveN(6)

      actor ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdated]
      actor ! RestartActor

      val restAmount = ord1.amount + ord2.amount + ord3.amount - ord4.amount
      tp.expectMsg(
        OrderAdded(
          SellLimitOrder(
            restAmount,
            ord2.matcherFee - LimitOrder.partialFee(ord2.matcherFee, ord2.amount, ord2.amount - restAmount),
            ord2
          ),
          ord2.timestamp
        ))
    }

    "place orders and restart without waiting for response" in obcTest { (pair, orderBook, tp) =>
      val ord1 = sell(pair, 10 * Order.PriceConstant, 100)
      val ts   = System.currentTimeMillis()

      (1 to 100) foreach { i =>
        orderBook ! wrap(ord1.updateTimestamp(ts + i))
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
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      (1 to 10).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      all(receiveN(10)) shouldBe AlreadyProcessed
    }

    "respond on SaveSnapshotCommand" in obcTest { (pair, actor, tp) =>
      (1 to 10).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(10)
      tp.expectMsg(OrderBookSnapshotUpdated(pair, 10))

      (11 to 20).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(20)
      tp.expectMsg(OrderBookSnapshotUpdated(pair, 20))
    }

    "don't do a snapshot if there is no changes" in obcTest { (pair, actor, tp) =>
      (1 to 10).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(10)
      actor ! SaveSnapshot(10)
      tp.expectMsgType[OrderBookSnapshotUpdated]
      tp.expectNoMessage(200.millis)
    }

    "restore its state at start" in obcTest { (pair, actor, tp) =>
      (1 to 10).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(10)
      tp.expectMsgType[OrderBookSnapshotUpdated]
    }

    "cancel order in merge small prices mode" in obcTestWithTickSize(TickSize.Enabled(100)) { (pair, orderBook, tp) =>
      val buyOrder = buy(pair, 100000000, 0.0000041)

      orderBook ! wrap(1, buyOrder)
      tp.expectMsgType[OrderAdded]

      orderBook ! wrap(2, Canceled(buyOrder.assetPair, buyOrder.id()))
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
        orderBook ! wrap(i, buyOrder)
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
        orderBook ! wrap(i, buyOrder)
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

      orderBook ! wrap(0, buyOrder1) // order book places order to the price level 41
      tp.expectMsgType[OrderAdded]

      orderBook ! wrap(1, buyOrder2) // now order book places the same order to the price level 40
      tp.expectMsgType[OrderAdded]

      eventually {
        val bids = obc.get(pair).bids
        bids.size shouldBe 2
        bids.head.price shouldBe buyOrder1.price
        bids.last.price shouldBe 0.0000040 * Order.PriceConstant
      }

      orderBook ! wrap(2, Canceled(buyOrder1.assetPair, buyOrder1.id())) // order book is looking for the price level of buyOrder1 correctly (41 but not 40)
      tp.expectMsgType[OrderCanceled]

      eventually {
        val bids = obc.get(pair).bids
        bids.size shouldBe 1
        bids.head.price shouldBe 0.000004 * Order.PriceConstant
      }
    }
  }
}
