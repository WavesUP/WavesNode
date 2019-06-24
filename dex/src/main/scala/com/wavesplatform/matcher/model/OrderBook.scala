package com.wavesplatform.matcher.model

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.matcher.model.Events._
import com.wavesplatform.matcher.model.MatcherModel.Price
import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}
import com.wavesplatform.utils.ScorexLogging
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.annotation.tailrec
import scala.collection.mutable

class OrderBook private (private[OrderBook] val bids: OrderBook.Side, private[OrderBook] val asks: OrderBook.Side) {

  import OrderBook._

  private[model] def getBids: OrderBook.Side = bids
  private[model] def getAsks: OrderBook.Side = asks

  def bestBid: Option[LevelAgg] = bids.aggregated.headOption
  def bestAsk: Option[LevelAgg] = asks.aggregated.headOption

  def allOrders: Iterable[(Long, LimitOrder)] = {
    for {
      (price, level) <- (bids: Iterable[(Price, Level)]) ++ asks
      lo             <- level
    } yield price -> lo
  }

  def cancel(orderId: ByteStr, timestamp: Long): Option[OrderCanceled] = {
    allOrders.collectFirst {
      case (price, lo) if lo.order.id() == orderId =>
        (if (lo.order.orderType == OrderType.BUY) bids else asks).remove(price, lo.order.id())
        OrderCanceled(lo, false, timestamp)
    }
  }

  def cancelAll(timestamp: Long): Seq[OrderCanceled] = {
    val canceledOrders = allOrders.map { case (_, lo) => OrderCanceled(lo, isSystemCancel = false, timestamp) }.toSeq
    bids.clear()
    asks.clear()
    canceledOrders
  }

  def add(ao: AcceptedOrder, ts: Long, tickSize: TickSize = TickSize.Disabled): Seq[Event] = {
    (
      ao.order.orderType match {
        case OrderType.BUY  => doMatch(ts, canMatchBuy, ao, Seq.empty, bids, asks, tickSize)
        case OrderType.SELL => doMatch(ts, canMatchSell, ao, Seq.empty, asks, bids, tickSize)
      }
    ).reverse
  }

  def snapshot: Snapshot                     = Snapshot(bids.toMap, asks.toMap)
  def aggregatedSnapshot: AggregatedSnapshot = AggregatedSnapshot(bids.aggregated.toSeq, asks.aggregated.toSeq)

  override def toString: String = s"""{"bids":${formatSide(bids)},"asks":${formatSide(asks)}}"""
}

object OrderBook extends ScorexLogging {

  type Level        = Vector[LimitOrder]
  type Side         = mutable.TreeMap[Price, Level]
  type SideSnapshot = Map[Price, Seq[LimitOrder]]

  case class Snapshot(bids: SideSnapshot, asks: SideSnapshot)

  case class AggregatedSnapshot(bids: Seq[LevelAgg] = Seq.empty, asks: Seq[LevelAgg] = Seq.empty) {
    def getSideFor(acceptedOrder: AcceptedOrder): Seq[LevelAgg]        = if (acceptedOrder.isBuyOrder) bids else asks
    def getCounterSideFor(acceptedOrder: AcceptedOrder): Seq[LevelAgg] = if (acceptedOrder.isBuyOrder) asks else bids
  }

  implicit class SideExt(val side: Side) extends AnyVal {

    def best: Option[LimitOrder] = side.headOption.flatMap(_._2.headOption)

    final def removeBest(): LimitOrder = side.headOption match {
      case l if l.forall(_._2.isEmpty) => throw new IllegalArgumentException("Cannot remove the best element from an empty level")
      case Some((price, level)) =>
        if (level.length == 1) side -= price
        else side += price -> level.tail
        level.head
    }

    def replaceBest(newBest: LimitOrder): Side = {
      require(side.nonEmpty, "Cannot replace the best level of an empty side")
      val (price, level) = side.head
      require(level.nonEmpty, "Cannot replace the best element of an empty level")
      side += (price -> (newBest +: level.tail))
    }

    def remove(price: Price, orderId: ByteStr): LimitOrder = {
      val (toRemove, toKeep) = side.getOrElse(price, Vector.empty).partition(_.order.id() == orderId)
      require(toRemove.nonEmpty, s"Order $orderId not found at $price")
      if (toKeep.isEmpty) side -= price else side += price -> toKeep

      toRemove.head
    }

    def aggregated: Iterable[LevelAgg] = for { (p, l) <- side.view if l.nonEmpty } yield LevelAgg(l.map(_.amount).sum, p)
  }

  private object canMatchBuy extends ((Long, Long) => Boolean) {
    def apply(submittedPrice: Long, counterPrice: Long): Boolean = submittedPrice >= counterPrice
    override val toString                                        = "submitted >= counter"
  }

  private object canMatchSell extends ((Long, Long) => Boolean) {
    def apply(submittedPrice: Long, counterPrice: Long): Boolean = submittedPrice <= counterPrice
    override val toString                                        = "submitted <= counter"
  }

  sealed trait TickSize
  object TickSize {
    case object Disabled                      extends TickSize
    case class Enabled(normalizedValue: Long) extends TickSize
  }

  private def correctPriceByTickSize(price: Price, orderType: OrderType, tickSize: TickSize): Price = tickSize match {
    case TickSize.Disabled => price
    case TickSize.Enabled(normalizedValue) =>
      if (price % normalizedValue == 0) price
      else
        orderType match {
          case OrderType.BUY  => price / normalizedValue * normalizedValue
          case OrderType.SELL => (price / normalizedValue + 1) * normalizedValue
        }
  }

  /** @param canMatch (submittedPrice, counterPrice) => Boolean */
  @tailrec
  private def doMatch(
      eventTs: Long,
      canMatch: (Long, Long) => Boolean,
      submitted: AcceptedOrder,
      prevEvents: Seq[Event],
      submittedSide: Side,
      counterSide: Side,
      tickSize: TickSize
  ): Seq[Event] = {
    if (!submitted.order.isValid(eventTs)) OrderCanceled(submitted, isSystemCancel = false, eventTs) +: prevEvents
    else
      counterSide.best match {
        case counter if counter.forall(c => !canMatch(submitted.price, c.price)) =>
          submitted.fold { submittedLimitOrder =>
            // place limit order into appropriate level according to the tick size
            val correctedPrice = correctPriceByTickSize(submittedLimitOrder.price, submittedLimitOrder.order.orderType, tickSize)

            submittedSide += correctedPrice -> (submittedSide.getOrElse(correctedPrice, Vector.empty) :+ submittedLimitOrder)
            OrderAdded(submittedLimitOrder, eventTs) +: prevEvents

          } { submittedMarketOrder =>
            // cancel market order in the absence of counters
            OrderCanceled(submittedMarketOrder, isSystemCancel = true, eventTs) +: prevEvents
          }
        case Some(counter) =>
          if (!submitted.isValid(counter.price)) OrderCanceled(submitted, isSystemCancel = true, eventTs) +: prevEvents
          else if (!counter.order.isValid(eventTs)) {

            counterSide.removeBest()
            doMatch(eventTs, canMatch, submitted, OrderCanceled(counter, false, eventTs) +: prevEvents, submittedSide, counterSide, tickSize)

          } else {

            val orderExecutedEvent = OrderExecuted(submitted, counter, eventTs)
            val newEvents          = orderExecutedEvent +: prevEvents

            if (orderExecutedEvent.counterRemaining.isValid) { // counter is not filled

              counterSide.replaceBest(orderExecutedEvent.counterRemaining)
              val submittedRemaining = orderExecutedEvent.submittedRemaining
              // if submitted is not filled (e.g. LimitOrder: rounding issues, MarkerOrder: afs = 0) cancel its remaining
              if (submittedRemaining.isValid) OrderCanceled(submittedRemaining, isSystemCancel = true, eventTs) +: newEvents else newEvents

            } else { // counter is filled

              counterSide.removeBest()

              submitted match {
                case _: LimitOrder =>
                  val submittedRemaining = orderExecutedEvent.submittedRemaining
                  if (submittedRemaining.isValid) doMatch(eventTs, canMatch, submittedRemaining, newEvents, submittedSide, counterSide, tickSize)
                  else newEvents
                case submittedMarketOrder: MarketOrder =>
                  val submittedRemaining = orderExecutedEvent.submittedMarketRemaining(submittedMarketOrder)
                  val isSubmittedFilled  = !submittedRemaining.isValid
                  val canSpendMore       = submittedRemaining.availableForSpending > 0
                  (isSubmittedFilled, canSpendMore) match {
                    case (false, true)  => doMatch(eventTs, canMatch, submittedRemaining, newEvents, submittedSide, counterSide, tickSize)
                    case (false, false) => OrderCanceled(submittedRemaining, isSystemCancel = true, eventTs) +: newEvents
                    case (true, _)      => newEvents
                  }
              }
            }
          }
      }
  }

  private def formatSide(side: Side) =
    side
      .map { case (price, level) => s""""$price":${level.map(formatLo).mkString("[", ",", "]")}""" }
      .mkString("{", ",", "}")

  // Showing owner for old orders. Should be deleted in Order.MaxLiveTime
  private def formatLo(lo: LimitOrder): String = s"""{"id":"${lo.order.id()}","owner":"${lo.order.senderPublicKey.toAddress.stringRepr}"}"""

  val bidsOrdering: Ordering[Long] = (x: Long, y: Long) => -Ordering.Long.compare(x, y)
  val asksOrdering: Ordering[Long] = (x: Long, y: Long) => Ordering.Long.compare(x, y)

  import com.wavesplatform.transaction.assets.exchange.OrderJson.orderFormat

  private def limitOrder(remainingAmount: Long, remainingFee: Long, o: Order): LimitOrder = o.orderType match {
    case OrderType.BUY  => BuyLimitOrder(remainingAmount, remainingFee, o)
    case OrderType.SELL => SellLimitOrder(remainingAmount, remainingFee, o)
  }

  private implicit val limitOrderFormat: Format[LimitOrder] = Format(
    Reads[LimitOrder] {
      case js: JsObject =>
        val amount = (js \ "amount").as[Long]
        val order  = (js \ "order").as[Order]
        val fee    = (js \ "fee").asOpt[Long].getOrElse(AcceptedOrder.partialFee(order.matcherFee, order.amount, amount))
        JsSuccess(limitOrder(amount, fee, order))
      case _ => JsError("failed to deserialize LimitOrder")
    },
    ((__ \ "amount").format[Long] and
      (__ \ "fee").format[Long] and
      (__ \ "order").format[Order])(limitOrder, (lo: LimitOrder) => (lo.amount, lo.fee, lo.order))
  )

  /*
  // Replace by:
  private implicit val limitOrderFormat: Format[LimitOrder] = (
    (JsPath \ "amount").format[Long] and
      (JsPath \ "fee").format[Long] and
      (JsPath \ "order").format[Order]
  )(limitOrder, lo => (lo.amount, lo.fee, lo.order))
   */

  implicit val priceMapFormat: Format[SideSnapshot] =
    implicitly[Format[Map[String, Seq[LimitOrder]]]].inmap(
      _.map { case (k, v) => k.toLong   -> v },
      _.map { case (k, v) => k.toString -> v }
    )

  implicit val snapshotFormat: Format[OrderBook.Snapshot] = Json.format

  def empty: OrderBook = new OrderBook(mutable.TreeMap.empty(bidsOrdering), mutable.TreeMap.empty(asksOrdering))

  private def transformSide(side: SideSnapshot, expectedSide: OrderType, ordering: Ordering[Long]): Side = {
    val bidMap = mutable.TreeMap.empty[Price, Level](ordering)
    for ((p, level) <- side) {
      val v = Vector.newBuilder[LimitOrder]
      for (lo <- level) {
        require(lo.order.orderType == expectedSide,
                s"Expecting $expectedSide only orders in bid list, but ${lo.order.id()} is a ${lo.order.orderType} order")
        v += lo
      }
      bidMap += p -> v.result()
    }
    bidMap
  }

  def apply(snapshot: Snapshot): OrderBook =
    new OrderBook(transformSide(snapshot.bids, OrderType.BUY, bidsOrdering), transformSide(snapshot.asks, OrderType.SELL, asksOrdering))
}
