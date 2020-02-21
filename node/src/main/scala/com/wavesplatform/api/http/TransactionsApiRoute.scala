package com.wavesplatform.api.http

import akka.NotUsed
import akka.http.scaladsl.common.EntityStreamingSupport
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.Source
import cats.instances.either._
import cats.instances.list._
import cats.instances.try_._
import cats.syntax.alternative._
import cats.syntax.either._
import cats.syntax.traverse._
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.api.http.ApiError._
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.TransactionProof
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.protobuf.api.TransactionsByIdRequest
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.lease._
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import monix.eval.Coeval
import monix.execution.Scheduler
import monix.reactive.Observable
import play.api.libs.json._

import scala.util.Success

case class TransactionsApiRoute(
    settings: RestAPISettings,
    commonApi: CommonTransactionsApi,
    wallet: Wallet,
    blockchain: Blockchain,
    utxPoolSize: Coeval[Int],
    utxPoolSynchronizer: UtxPoolSynchronizer,
    time: Time
) extends ApiRoute
    with BroadcastRoute
    with AuthRoute
    with AutoParamsDirective {
  import TransactionsApiRoute._

  override lazy val route: Route =
    pathPrefix("transactions") {
      unconfirmed ~ addressLimit ~ info ~ status ~ sign ~ calculateFee ~ signedBroadcast ~ merkleProof
    }

  def addressLimit: Route = {
    (get & path("address" / AddrSegment / "limit" / IntNumber) & parameter('after.?)) { (address, limit, maybeAfter) =>
      val after =
        maybeAfter.map(s => ByteStr.decodeBase58(s).getOrElse(throw ApiException(CustomValidationError(s"Unable to decode transaction id $s"))))
      if (limit > settings.transactionsByAddressLimit) throw ApiException(TooBigArrayAllocation)
      extractScheduler { implicit sc =>
        implicit val jsonStreamingSupport: EntityStreamingSupport = jsonStream("[[", ",", "]]")
        complete(transactionsByAddress(address, limit, after))
      }
    }
  }

  def info: Route = (pathPrefix("info") & get) {
    pathEndOrSingleSlash {
      complete(InvalidTransactionId("Transaction ID was not specified"))
    } ~ path(TransactionId) { id =>
      commonApi.transactionById(id) match {
        case Some((h, either)) => complete(txToExtendedJson(either.fold(identity, _._1)) + ("height" -> JsNumber(h)))
        case None              => complete(ApiError.TransactionDoesNotExist)
      }
    }
  }

  def status: Route = path("status") {
    protobufEntity(TransactionsByIdRequest) { request =>
      if (request.ids.length > settings.transactionsByAddressLimit)
        complete(TooBigArrayAllocation)
      else {
        request.ids.map(id => ByteStr.decodeBase58(id).toEither.leftMap(_ => id)).toList.separate match {
          case (Nil, Nil) => complete(CustomValidationError("Empty request"))
          case (Nil, ids) =>
            val results = ids.toSet.map { id: ByteStr =>
              val statusJson = blockchain.transactionInfo(id) match {
                case Some((height, _)) =>
                  Json.obj("status" -> "confirmed", "height" -> height, "confirmations" -> (blockchain.height - height).max(0))

                case None =>
                  commonApi.unconfirmedTransactionById(id) match {
                    case Some(_) => Json.obj("status" -> "unconfirmed")
                    case None    => Json.obj("status" -> "not_found")
                  }
              }
              id -> (statusJson ++ Json.obj("id" -> id.toString))
            }.toMap
            complete(ids.map(id => results(id)))
          case (errors, _) => complete(InvalidIds(errors))
        }
      }
    }
  }

  def unconfirmed: Route = (pathPrefix("unconfirmed") & get) {
    pathEndOrSingleSlash {
      complete(JsArray(commonApi.unconfirmedTransactions.map(txToExtendedJson)))
    } ~ utxSize ~ utxTransactionInfo
  }

  def utxSize: Route = (pathPrefix("size") & get) {
    complete(Json.obj("size" -> JsNumber(utxPoolSize())))
  }

  def utxTransactionInfo: Route = (pathPrefix("info") & get) {
    pathEndOrSingleSlash {
      complete(InvalidSignature)
    } ~
      path(TransactionId) { id =>
        commonApi.unconfirmedTransactionById(id) match {
          case Some(tx) =>
            complete(txToExtendedJson(tx))
          case None =>
            complete(ApiError.TransactionDoesNotExist)
        }
      }
  }

  def calculateFee: Route =
    path("calculateFee")(jsonPost[JsObject] { jsv =>
      val senderPk = (jsv \ "senderPublicKey").as[String]
      // Just for converting the request to the transaction
      val enrichedJsv = jsv ++ Json.obj(
        "fee"    -> 1234567,
        "sender" -> senderPk
      )

      createTransaction(senderPk, enrichedJsv) { tx =>
        commonApi
          .calculateFee(tx)
          .map { case (assetId, assetAmount, _) => Json.obj("feeAssetId" -> assetId, "feeAmount" -> assetAmount) }
      }
    })

  def sign: Route = (pathPrefix("sign") & withAuth) {
    pathEndOrSingleSlash(jsonPost[JsObject] { jsv =>
      TransactionFactory.parseRequestAndSign(wallet, (jsv \ "sender").as[String], time, jsv)
    }) ~ signWithSigner
  }

  def signWithSigner: Route = path(AddrSegment) { address =>
    jsonPost[JsObject](TransactionFactory.parseRequestAndSign(wallet, address.stringRepr, time, _))
  }

  def signedBroadcast: Route = path("broadcast")(broadcast[JsValue](TransactionFactory.fromSignedRequest))

  def merkleProof: Route = path("merkleProof") {
    (get & parameters('id.*))(ids => complete(merkleProof(ids.toList.reverse))) ~
      jsonPost[JsObject](
        jsv =>
          (jsv \ "ids").validate[List[String]] match {
            case JsSuccess(ids, _) => merkleProof(ids)
            case JsError(err)      => WrongJson(errors = err)
          }
      )
  }

  private def merkleProof(encodedIds: List[String]): ToResponseMarshallable =
    encodedIds.traverse(ByteStr.decodeBase58) match {
      case Success(txIds) =>
        commonApi.transactionProofs(txIds) match {
          case Nil    => CustomValidationError(s"transactions do not exist or block version < ${Block.ProtoBlockVersion}")
          case proofs => proofs
        }
      case _ => InvalidSignature
    }

  private def txToExtendedJson(tx: Transaction): JsObject = {
    import com.wavesplatform.transaction.lease.LeaseTransaction
    tx match {
      case lease: LeaseTransaction =>
        import com.wavesplatform.api.http.TransactionsApiRoute.LeaseStatus._
        lease.json() ++ Json.obj("status" -> (if (blockchain.leaseDetails(lease.id()).exists(_.isActive)) Active else Canceled))

      case leaseCancel: LeaseCancelTransaction =>
        leaseCancel.json() ++ Json.obj("lease" -> blockchain.transactionInfo(leaseCancel.leaseId).map(_._2.json()).getOrElse[JsValue](JsNull))

      case t => t.json()
    }
  }

  def transactionsByAddress(address: Address, limitParam: Int, maybeAfter: Option[ByteStr])(implicit sc: Scheduler): Source[JsObject, NotUsed] =
    Source.fromPublisher(
      Observable
        .fromTask(commonApi.aliasesOfAddress(address).collect { case (_, cat) => cat.alias }.toListL)
        .flatMap { aliases =>
          val addressesCached = (aliases :+ address).toSet

          /**
            * Produces compact representation for large transactions by stripping unnecessary data.
            * Currently implemented for MassTransfer transaction only.
            */
          def txToCompactJson(address: Address, tx: Transaction): JsObject = {
            import com.wavesplatform.transaction.transfer._
            tx match {
              case mtt: MassTransferTransaction if mtt.sender.toAddress != address => mtt.compactJson(addressesCached)
              case _                                                               => txToExtendedJson(tx)
            }
          }

          commonApi
            .transactionsByAddress(address, None, Set.empty, maybeAfter)
            .take(limitParam)
            .map { case (height, tx) => txToCompactJson(address, tx) + ("height" -> JsNumber(height)) }
        }
        .toReactivePublisher
    )
}

object TransactionsApiRoute {
  object LeaseStatus {
    val Active   = "active"
    val Canceled = "canceled"
  }

  implicit val transactionProofWrites: Writes[TransactionProof] = Writes { mi =>
    Json.obj(
      "id"               -> mi.id.toString,
      "transactionIndex" -> mi.transactionIndex,
      "merkleProof"      -> mi.digests.map(d => s"${Base58.encode(d)}")
    )
  }

  implicit val transactionProofReads: Reads[TransactionProof] = Reads { jsv =>
    for {
      encoded          <- (jsv \ "id").validate[String]
      id               <- ByteStr.decodeBase58(encoded).fold(_ => JsError(InvalidSignature.message), JsSuccess(_))
      transactionIndex <- (jsv \ "transactionIndex").validate[Int]
      merkleProof      <- (jsv \ "merkleProof").validate[List[String]].map(_.map(Base58.decode))
    } yield TransactionProof(id, transactionIndex, merkleProof)
  }
}
