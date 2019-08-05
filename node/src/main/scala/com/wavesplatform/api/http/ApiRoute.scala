package com.wavesplatform.api.http

import java.util.NoSuchElementException
import java.util.concurrent.ExecutionException

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server._
import com.wavesplatform.api.http.ApiError.{ApiErrorException, ApiKeyNotValid, WrongJson}
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto
import com.wavesplatform.http.{ApiMarshallers, PlayJsonException, `X-Api-Key`, api_key}
import com.wavesplatform.lang.ValidationError.ValidationErrorException
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utils.ScorexLogging
import monix.execution.Scheduler
import play.api.libs.json.{JsResultException, Reads}

import scala.concurrent.Future
import scala.util.control.NonFatal

trait ApiRoute extends Directives with CommonApiFunctions with ApiMarshallers with ScorexLogging {
  protected val route: Route

  protected def apiKeyHash: Option[Array[Byte]]

  def getRoute: Route = handleAnyExceptions(uncaughtExceptionHandler)(route)

  private val jsonRejectionHandler = RejectionHandler
    .newBuilder()
    .handle { case ValidationRejection(_, Some(PlayJsonException(cause, errors))) => complete(WrongJson(cause, errors)) }
    .result()

  def jsonEntity[A: Reads]: Directive1[A]                   = handleRejections(jsonRejectionHandler) & entity(as[A])
  def json[A: Reads](f: A => ToResponseMarshallable): Route = jsonEntity.apply(a => complete(f(a)))

  val jsonExceptionHandler: ExceptionHandler = ExceptionHandler {
    case JsResultException(err)                                         => complete(WrongJson(errors = err))
    case PlayJsonException(cause, errors)                               => complete(WrongJson(cause, errors))
    case e: NoSuchElementException                                      => complete(WrongJson(Some(e)))
    case e: ApiErrorException                                           => complete(e.error)
    case e: ValidationErrorException                                    => complete(ApiError.fromValidationError(e.error))
    case e: IllegalArgumentException                                    => complete(ApiError.fromValidationError(GenericError(e)))
    case e: AssertionError                                              => complete(ApiError.fromValidationError(GenericError(e)))
    case e: ExecutionException if e.getCause != null && e.getCause != e => jsonExceptionHandler(e.getCause)
  }

  private val uncaughtExceptionHandler: ExceptionHandler = ExceptionHandler {
    case e: StackOverflowError => log.error("Stack overflow error", e); complete(ApiError.Unknown)
    case NonFatal(e)           => log.error("Uncaught error", e); complete(ApiError.Unknown)
  }

  private def handleAnyExceptions(handler: ExceptionHandler): Directive0 =
    Directive { inner => ctx =>
      val handleExceptions = handler.andThen(_(ctx))
      try inner(())(ctx).recoverWith(handleExceptions)(ctx.executionContext)
      catch {
        case thr: Throwable => handler.andThen(_(ctx)).applyOrElse[Throwable, Future[RouteResult]](thr, throw _)
      }
    }


  def withAuth: Directive0 = apiKeyHash.fold[Directive0](complete(ApiKeyNotValid)) { hashFromSettings =>
    optionalHeaderValueByType[`X-Api-Key`](()).flatMap {
      case Some(k) if java.util.Arrays.equals(crypto.secureHash(k.value.getBytes("UTF-8")), hashFromSettings) => pass
      case _ =>
        optionalHeaderValueByType[api_key](()).flatMap {
          case Some(k) if java.util.Arrays.equals(crypto.secureHash(k.value.getBytes("UTF-8")), hashFromSettings) => pass
          case _                                                                                           => complete(ApiKeyNotValid)
        }
    }
  }

  def processRequest[A: Reads](pathMatcher: String, f: A => ToResponseMarshallable): Route =
    (path(pathMatcher) & post & withAuth) {
      json[A](f)
    }

  def extractScheduler: Directive1[Scheduler] = extractExecutionContext.map(ec => Scheduler(ec))
}

trait WithSettings { this: ApiRoute =>
  def settings: RestAPISettings
  protected override lazy val apiKeyHash: Option[Array[Byte]] = Base58.tryDecode(settings.apiKeyHash).toOption
}
