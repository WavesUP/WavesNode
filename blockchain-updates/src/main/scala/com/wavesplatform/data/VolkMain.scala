package com.wavesplatform.data

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.lang.script.{Script, ScriptReader}
import com.wavesplatform.protobuf.transaction.{PBTransactions, SetScriptTransactionData, Transaction}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utils.ScorexLogging
import okhttp3.{MediaType, OkHttpClient, Request, RequestBody}
import play.api.libs.json.Json

import scala.util.control.NonFatal

object VolkMain extends App with ScorexLogging {
  object Channels {
    val sranb1 = "https://hooks.slack.com/services/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"
    lazy val env = sys.env("VOLK_CHANNELS").split(";")
  }

//  val matches = Seq("4LHHvYGNKJUg5hj65aGD5vgScvCBmLpdRFtjokvCjSL8")
    val matches = sys.env.getOrElse("VOLK_MATCHES", "").split(";")

  def sendNotification(channel: String, height: Int, txId: ByteStr, text: String): Unit = {
    val httpClient = new OkHttpClient.Builder().build()
    val matchesFound = matches.flatMap(m => m.r.findAllMatchIn(text))
    val call = httpClient.newCall(
      new Request.Builder()
        .url(channel)
        .post(
          RequestBody.create(
            MediaType.parse("application/json"),
            Json.obj("text" ->
              s"""${if (matchesFound.nonEmpty) "@channel" else ""}
                 |New script discovered!
                 |Height: *$height*
                 |Transaction: `$txId`${if (matchesFound.nonEmpty) s"\nMatches: ${matchesFound.map(m => "_" + m + "_").mkString(", ")}" else ""}
                 |```
                 |$text
                 |```
                 |""".stripMargin, "link_names" -> 1).toString()
          )
        )
        .build()
    )
    val response = call.execute()
    log.info(s"Notified: $response")
    response.close()
  }

  val pa = new PollingAgent
  pa.start(_.foreach {
    case BlockchainUpdated(
        _,
        height,
        BlockchainUpdated.Update.Append(BlockchainUpdated.Append(_, _, _, body))
        ) =>
      log.info(s"Block event at $height: ${body.getClass}")

      val txs = body match {
        case Body.Block(value)      => value.transactions
        case Body.MicroBlock(value) => value.getMicroBlock.transactions
        case Body.Empty             => Nil
      }

      txs.foreach { tx =>
        val script = tx.getTransaction.data match {
          case Transaction.Data.SetScript(SetScriptTransactionData(Some(script))) => Some(script)
          case _ => None
        }

        for {
          rawScript <- script.toRight(GenericError(""))
          script <- ScriptReader.fromBytes(
            rawScript.bytes.toByteArray,
            checkComplexity = false
          )
          vtx <- PBTransactions.vanilla(tx, unsafe = true)
        } try {
          val (scriptText, _) = Script.decompile(script)
          log.info(s"Script decompiled: $scriptText")
          Channels.env.foreach(sendNotification(_, height, vtx.id(), scriptText))
        } catch {
          case NonFatal(e) =>
            log.info(s"Error decompiling script: $rawScript", e)
        }
      }

    case _ => // Ignore
  })
}
