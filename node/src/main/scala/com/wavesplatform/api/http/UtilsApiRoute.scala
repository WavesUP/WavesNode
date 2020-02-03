package com.wavesplatform.api.http

import java.security.SecureRandom

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import com.wavesplatform.account.PrivateKey
import com.wavesplatform.api.http.ApiError.{ScriptCompilerError, TooBigArrayAllocation}
import com.wavesplatform.common.utils._
import com.wavesplatform.crypto
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.utils.Time
import io.swagger.annotations._
import javax.ws.rs.Path
import monix.execution.Scheduler
import play.api.libs.json._

@Path("/utils")
@Api(value = "/utils", description = "Useful functions", position = 3, produces = "application/json")
case class UtilsApiRoute(
    timeService: Time,
    settings: RestAPISettings,
    estimator: ScriptEstimator,
    limitedScheduler: Scheduler
) extends ApiRoute
    with AuthRoute
    with TimeLimitedRoute {

  import UtilsApiRoute._

  private def seed(length: Int) = {
    val seed = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    Json.obj("seed" -> Base58.encode(seed))
  }

  override val route: Route = pathPrefix("utils") {
    decompile ~ compile ~ compileCode ~ compileWithImports ~ scriptMeta ~ estimate ~ time ~ seedRoute ~ length ~ hashFast ~ hashSecure ~ sign ~ transactionSerialize
  }

  @Path("/script/decompile")
  @ApiOperation(value = "Decompile", notes = "Decompiles base64 script representation to string code", httpMethod = "POST", response = classOf[DecompiledScriptDesc])
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "code",
        required = true,
        dataType = "string",
        paramType = "body",
        value = "Script code",
        example = "true"
      )
    )
  )
  def decompile: Route = path("script" / "decompile") {
    import play.api.libs.json.Json.toJsFieldJsValueWrapper

    (post & entity(as[String])) { code =>
      Script.fromBase64String(code.trim) match {
        case Left(err) => complete(err)
        case Right(script) =>
          executeLimited(Script.decompile(script)) {
            case (scriptText, meta) =>
              val directives: List[(String, JsValue)] = meta.map {
                case (k, v) =>
                  (k, v match {
                    case n: Number => JsNumber(BigDecimal(n.toString))
                    case s         => JsString(s.toString)
                  })
              }
              val result  = directives ::: "script" -> JsString(scriptText) :: Nil
              val wrapped = result.map { case (k, v) => (k, toJsFieldJsValueWrapper(v)) }
              complete(
                Json.obj(wrapped: _*)
              )
          }
      }
    }
  }

  @Deprecated
  @Path("/script/compile")
  @ApiOperation(value = "Compile", notes = "Compiles string code to base64 script representation", httpMethod = "POST", response = classOf[CompiledScriptDesc])
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "code",
        required = true,
        dataType = "string",
        paramType = "body",
        value = "Script code",
        example = "true"
      )
    )
  )
  def compile: Route = path("script" / "compile") {
    (post & entity(as[String])) { code =>
      parameter('assetScript.as[Boolean] ? false) { isAssetScript =>
        executeLimited(ScriptCompiler(code, isAssetScript, estimator)) { result =>
          complete(
            result.fold(
              e => ScriptCompilerError(e), {
                case (script, complexity) =>
                  Json.obj(
                    "script"     -> script.bytes().base64,
                    "complexity" -> complexity,
                    "extraFee"   -> FeeValidation.ScriptExtraFee
                  )
              }
            )
          )
        }
      }
    }
  }

  @Path("/script/compileCode")
  @ApiOperation(value = "Compile script", notes = "Compiles string code to base64 script representation", httpMethod = "POST", response = classOf[CompiledScriptDesc])
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "code",
        required = true,
        dataType = "string",
        paramType = "body",
        value = "Script code",
        example = "true"
      )
    )
  )
  def compileCode: Route = path("script" / "compileCode") {
    (post & entity(as[String])) { code =>
      executeLimited(ScriptCompiler.compile(code, estimator)) { result =>
        complete(
          result
            .fold(
              e => ScriptCompilerError(e), {
                case (script, complexity) =>
                  Json.obj(
                    "script"     -> script.bytes().base64,
                    "complexity" -> complexity,
                    "extraFee"   -> FeeValidation.ScriptExtraFee
                  )
              }
            )
        )

      }

    }
  }

  @Path("/script/compileWithImports")
  @ApiOperation(value = "Compile script", notes = "Compiles string code with imports to base64 script representation", httpMethod = "POST", response = classOf[CompiledScriptDesc])
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "code",
        required = true,
        dataTypeClass = classOf[ScriptWithImportsRequest],
        paramType = "body",
        value = "Script code with imports"
      )
    )
  )
  def compileWithImports: Route = path("script" / "compileWithImports") {
    import ScriptWithImportsRequest._
    (post & entity(as[ScriptWithImportsRequest])) { req =>
      executeLimited(ScriptCompiler.compile(req.script, estimator, req.imports)) { result =>
        complete(
          result
            .fold(
              e => ScriptCompilerError(e), {
                case (script, complexity) =>
                  Json.obj(
                    "script"     -> script.bytes().base64,
                    "complexity" -> complexity,
                    "extraFee"   -> FeeValidation.ScriptExtraFee
                  )
              }
            )
        )
      }
    }
  }

  @Path("/script/estimate")
  @ApiOperation(value = "Estimate", notes = "Estimates compiled code in Base64 representation", httpMethod = "POST", response = classOf[EstimateDesc])
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "code",
        required = true,
        dataType = "string",
        paramType = "body",
        value = "A compiled Base64 code",
        example = "AQa3b8tH"
      )
    )
  )
  def estimate: Route = path("script" / "estimate") {
    (post & entity(as[String])) { code =>
      executeLimited(
        Script
          .fromBase64String(code)
          .left
          .map(_.m)
          .flatMap { script =>
            Script.estimate(script, estimator).map((script, _))
          }
      ) { result =>
        complete(
          result.fold(
            e => ScriptCompilerError(e), {
              case (script, complexity) =>
                Json.obj(
                  "script"     -> code,
                  "scriptText" -> script.expr.toString, // [WAIT] Script.decompile(script),
                  "complexity" -> complexity,
                  "extraFee"   -> FeeValidation.ScriptExtraFee
                )
            }
          )
        )
      }
    }
  }

  @Path("/script/meta")
  @ApiOperation(value = "Meta", notes = "Account's script meta", httpMethod = "POST", hidden = true, response = classOf[Map[String, String]])
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "code",
        required = true,
        dataType = "string",
        paramType = "body",
        value = "Script code",
        example = "true"
      )
    )
  )
  def scriptMeta: Route = path("script" / "meta") {
    (post & entity(as[String])) { code =>
      val result: ToResponseMarshallable = Global
        .scriptMeta(code) // Does not estimate complexity, therefore it should not hang
        .map(metaConverter.foldRoot)
        .fold(e => e, r => r)
      complete(result)
    }
  }

  @Path("/time")
  @ApiOperation(value = "Time", notes = "Current Node time (UTC)", httpMethod = "GET", response = classOf[TimeDesc])
  def time: Route = (path("time") & get) {
    complete(Json.obj("system" -> System.currentTimeMillis(), "NTP" -> timeService.correctedTime()))
  }

  @Path("/seed")
  @ApiOperation(value = "Seed", notes = "Generate random seed", httpMethod = "GET", response = classOf[SeedDesc])
  def seedRoute: Route = (path("seed") & get) {
    complete(seed(DefaultSeedSize))
  }

  @Path("/seed/{length}")
  @ApiOperation(value = "Seed of specified length", notes = "Generate random seed of specified length", httpMethod = "GET", response = classOf[SeedDesc])
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "length", value = "Seed length ", required = true, dataType = "integer", paramType = "path")
    )
  )
  def length: Route = (path("seed" / IntNumber) & get) { length =>
    if (length <= MaxSeedSize) complete(seed(length))
    else complete(TooBigArrayAllocation)
  }

  @Path("/hash/secure")
  @ApiOperation(value = "Hash", notes = "Return SecureCryptographicHash of specified message", httpMethod = "POST", response = classOf[HashedMessageDesc])
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "message",
        value = "Message to hash",
        required = true,
        paramType = "body",
        dataType = "string"
      )
    )
  )
  def hashSecure: Route = (path("hash" / "secure") & post) {
    entity(as[String]) { message =>
      complete(Json.obj("message" -> message, "hash" -> Base58.encode(crypto.secureHash(message))))
    }
  }

  @Path("/hash/fast")
  @ApiOperation(value = "Hash", notes = "Return FastCryptographicHash of specified message", httpMethod = "POST", response = classOf[HashedMessageDesc])
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "message",
        value = "Message to hash",
        required = true,
        paramType = "body",
        dataType = "string"
      )
    )
  )
  def hashFast: Route = (path("hash" / "fast") & post) {
    entity(as[String]) { message =>
      complete(Json.obj("message" -> message, "hash" -> Base58.encode(crypto.fastHash(message))))
    }
  }
  @Path("/sign/{privateKey}")
  @ApiOperation(value = "Hash", notes = "Return FastCryptographicHash of specified message", httpMethod = "POST", response = classOf[SignedMessageDesc])
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "privateKey",
        value = "privateKey",
        required = true,
        paramType = "path",
        dataType = "string",
        example = "3kMEhU5z3v8bmer1ERFUUhW58Dtuhyo9hE5vrhjqAWYT"
      ),
      new ApiImplicitParam(
        name = "message",
        value = "Message to hash (base58 string)",
        required = true,
        paramType = "body",
        dataType = "string"
      )
    )
  )
  def sign: Route = (path("sign" / Segment) & post) { pk =>
    entity(as[String]) { message =>
      complete(
        Json.obj(
          "message" -> message,
          "signature" ->
            Base58.encode(crypto.sign(PrivateKey(Base58.tryDecodeWithLimit(pk).get), Base58.tryDecodeWithLimit(message).get))
        )
      )
    }
  }

  @Path("/transactionSerialize")
  @ApiOperation(value = "Serialize transaction", notes = "Serialize transaction", httpMethod = "POST", response = classOf[SerializedTransactionDesc])
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "json",
        required = true,
        paramType = "body",
        dataType = "string",
        value = "Transaction data including <a href='transaction-types.html'>type</a> and signature/proofs"
      )
    )
  )
  def transactionSerialize: Route =
    path("transactionSerialize")(jsonPost[JsObject] { jsv =>
      parseOrCreateTransaction(jsv)(tx => Json.obj("bytes" -> tx.bodyBytes().map(_.toInt & 0xff)))
    })

  private[this] case class DecompiledScriptDesc(script: String)
  private[this] case class CompiledScriptDesc(script: String, complexity: Int, extraFee: Long)
  private[this] case class EstimateDesc(script: String, scriptText: String, complexity: Int, extraFee: Long)
  private[this] case class TimeDesc(system: Long, NTP: Long)
  private[this] case class SeedDesc(seed: String)
  private[this] case class SignedMessageDesc(message: String, signature: String)
  private[this] case class HashedMessageDesc(message: String, hash: String)
  private[this] case class SerializedTransactionDesc(bytes: Array[Byte])
}

object UtilsApiRoute {
  val MaxSeedSize     = 1024
  val DefaultSeedSize = 32
}
