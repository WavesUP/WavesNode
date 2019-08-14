package com.wavesplatform.data

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives
import akka.stream.ActorMaterializer
import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.{BlockchainUpdated, StateUpdate}
import com.wavesplatform.protobuf.transaction.{PBRecipients, PBTransactions}
import com.wavesplatform.utils.ScorexLogging
import okhttp3.{MediaType, OkHttpClient, Request, RequestBody}
import play.api.libs.json.Json

import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object VolkMain extends App with ScorexLogging {

  object VolkDB {

    import io.getquill._

    private[this] lazy val ctx = new H2JdbcContext(SnakeCase, "volk.db")

    import ctx.{lift => liftQ, _}

    private[this] object Model {

      case class WatchedAddress(address: String, tag: String)

      implicit val watchedAddressMeta = schemaMeta[WatchedAddress]("watched_addresses")
    }

    import Model._

    def getTags(addr: String): Set[String] = {
      val q = quote {
        query[WatchedAddress].filter(_.address == liftQ(addr)).map(_.tag).distinct
      }
      run(q).toSet
    }

    def markAddress(addr: String, tags: Set[String]): Unit = {
      val entries = tags.toSeq.map((addr, _))
      val q = quote {
        liftQuery(entries).foreach { case (addr, tag) => query[WatchedAddress].insert(_.address -> addr, _.tag -> tag) }
      }
      try run(q) catch { case NonFatal(_) => }
    }
  }

  val server = new Directives {
    val route = (path("command") & formField("text")) { text =>
      val Array(addr, tagsStr) = text.split(" ", 2)
      val tags = tagsStr.split(",")
      VolkDB.markAddress(addr, tags.toSet)
      complete(StatusCodes.OK)
    }
  }

  object Channels {
    lazy val env = sys.env("VOLK_CHANNELS").split(";")
  }

  def sendNotification(channel: String, height: Int, txId: ByteStr, from: String, to: Seq[String], tags: Set[String]): Unit = {
    val httpClient = new OkHttpClient.Builder().build()
    val call = httpClient.newCall(
      new Request.Builder()
        .url(channel)
        .post(
          RequestBody.create(
            MediaType.parse("application/json"),
            Json.obj("text" ->
              s"""Transfer discovered!
                 |Height: *$height*
                 |Transaction: `$txId`
                 |Sender: `$from`
                 |Recipients: ${to.map("`" + _ + "`").mkString(", ")}
                 |Tags: ${tags.map("*" + _ + "*").mkString(", ")}
                 |""".stripMargin, "link_names" -> 1).toString()
          )
        )
        .build()
    )
    val response = call.execute()
    log.info(s"Notified: $response")
    response.close()
  }

  implicit val actorSystem = ActorSystem()
  implicit val materializer = ActorMaterializer()

  import actorSystem.dispatcher

  VolkDB.markAddress("3PGGv4a9LquYJ9SLU3JEh4g9caTJh6zuwKs", Set("test"))
  VolkDB.markAddress("3P23fi1qfVw6RVDn4CH2a5nNouEtWNQ4THs", Set("test"))
  VolkDB.markAddress("3PMT9wun7BB7JABSuhTJpFgJoegRfYw2e6d", Set("test"))
  VolkDB.markAddress("3PHxaBWnwaWSyvjzXHwWbDF5s4R4YDyZAhS", Set("test"))

  Http().bindAndHandle(server.route, "0.0.0.0", 1234).onComplete {
    case Success(value) =>
      log.info(s"Successfully bound to $value")

    case Failure(exception) =>
      log.error("Bind exception", exception)
  }

  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = 'W'.toByte
  }

  val senderMap = collection.mutable.Map.empty[String, Int]

  val pollingAgent = new PollingAgent
  pollingAgent.start(_.foreach {
    case BlockchainUpdated(
    _,
    height,
    BlockchainUpdated.Update.Append(BlockchainUpdated.Append(_, _, stateUpdates, body))
    ) =>
      log.info(s"Block event at $height: ${body.getClass}")

      val txs = body match {
        case Body.Block(value) => value.transactions
        case Body.MicroBlock(value) => value.getMicroBlock.transactions
        case Body.Empty => Nil
      }

      txs.foreach { tx =>
        val sender = PublicKey(tx.getTransaction.senderPublicKey.toByteArray).toAddress
        val tags = VolkDB.getTags(sender.stringRepr)

        // log.info(stateUpdates.toString())

        val affected = stateUpdates.flatMap {
          case StateUpdate(balances, _, _) =>
            balances.flatMap(b => PBRecipients.toAddress(b.address.toByteArray).toOption)
        }

        if (affected.nonEmpty) senderMap(sender.stringRepr) = senderMap.getOrElse(sender.stringRepr, 0) + 1

        if (tags.nonEmpty && affected.nonEmpty) {
          // log.info(s"State updates: $stateUpdates")
          affected.foreach(addr => VolkDB.markAddress(addr.stringRepr, tags))

          val txId = PBTransactions.vanilla(tx, unsafe = true).fold(_ => ByteStr.empty, _.id())
          Channels.env.foreach(sendNotification(_, height, txId, sender.stringRepr, affected.map(_.stringRepr), tags))
        }
      }

      log.info(senderMap.toSeq.sortBy(-_._2).take(3).map { case (a, c) => s"$a -> $c"}.mkString(", "))

    case _ => // Ignore
  })
}
