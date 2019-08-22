package com.wavesplatform.api.http

import akka.http.scaladsl.server.{Route, StandardRoute}
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.api.http.ApiError.{BlockDoesNotExist, CustomValidationError, InvalidSignature, TooBigArrayAllocation}
import com.wavesplatform.block.BlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction._
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json._

@Path("/blocks")
@Api(value = "/blocks")
case class BlocksApiRoute(settings: RestAPISettings, blockchain: Blockchain)
    extends ApiRoute with CommonApiFunctions {
  private[this] val MaxBlocksPerRequest = 100 // todo: make this configurable and fix integration tests
  private[this] val commonApi           = new CommonBlocksApi(blockchain)

  override lazy val route =
    pathPrefix("blocks") {
      signature ~ first ~ last ~ lastHeaderOnly ~ at ~ atHeaderOnly ~ seq ~ seqHeaderOnly ~ height ~ heightEncoded ~ child ~ address ~ delay
    }

  @Path("/address/{address}/{from}/{to}")
  @ApiOperation(value = "Blocks produced by address", notes = "Get list of blocks generated by specified address", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "from", value = "Start block height", required = true, dataType = "integer", paramType = "path"),
      new ApiImplicitParam(name = "to", value = "End block height", required = true, dataType = "integer", paramType = "path"),
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    ))
  def address: Route =
    extractScheduler(implicit sc =>
      (path("address" / Segment / IntNumber / IntNumber) & get) {
        case (address, start, end) =>
          if (end >= 0 && start >= 0 && end - start >= 0 && end - start < MaxBlocksPerRequest) {
            val result = for {
              address <- Address.fromString(address)
              jsonBlocks = commonApi.blockHeadersRange(start, end).filter(_._1.signerData.generator.toAddress == address)
              .map {
            case (_, _, h) =>
              blockchain.blockAt(h).get.json().addBlockFields(h)
          }
              result    = jsonBlocks.toListL.map(JsArray(_))
            } yield result.runToFuture

            complete(result)
          } else {
            complete(TooBigArrayAllocation)
          }
    })

  def child: Route = (path("child" / Segment) & get) { encodedSignature =>
    withBlock(blockchain, encodedSignature) { block =>
      val childJson = for ((child, height) <- commonApi.childBlock(block.uniqueId))
        yield child.json().addBlockFields(height)

      complete(childJson.getOrElse[JsObject](Json.obj("status" -> "error", "details" -> "No child blocks")))
    }
  }

  @Path("/delay/{signature}/{blockNum}")
  @ApiOperation(
    value = "Average block delay",
    notes = "Average delay in milliseconds between last `blockNum` blocks starting from block with `signature`",
    httpMethod = "GET"
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "signature", value = "Base58-encoded block signature", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "blockNum", value = "Number of blocks to count delay", required = true, dataType = "string", paramType = "path")
    ))
  def delay: Route = (path("delay" / Segment / IntNumber) & get) { (encodedSignature, count) =>
    withBlock(blockchain, encodedSignature) { block =>
      val result = if (count <= 0) {
        Left(CustomValidationError("Block count should be positive"))
      } else {
        commonApi
          .calcBlocksDelay(block.uniqueId, count)
          .map(delay => Json.obj("delay" -> delay))
      }

      complete(result)
    }
  }

  @Path("/height/{signature}")
  @ApiOperation(value = "Block height", notes = "Height of a block by its signature", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "signature", value = "Base58-encoded block signature", required = true, dataType = "string", paramType = "path")
    ))
  def heightEncoded: Route = (path("height" / Segment) & get) { encodedSignature =>
    if (encodedSignature.length > TransactionParsers.SignatureStringLength)
      complete(InvalidSignature)
    else {
      val result: Either[ApiError, JsObject] = for {
        signature <- ByteStr
          .decodeBase58(encodedSignature)
          .toOption
          .toRight(InvalidSignature)

        height <- commonApi.blockHeight(signature).toRight(BlockDoesNotExist)
      } yield Json.obj("height" -> height)

      complete(result)
    }
  }

  @Path("/height")
  @ApiOperation(value = "Blockchain height", notes = "Get current blockchain height", httpMethod = "GET")
  def height: Route = (path("height") & get) {
    complete(Json.obj("height" -> commonApi.currentHeight()))
  }

  @Path("/at/{height}")
  @ApiOperation(value = "Block at height", notes = "Get block at specified height", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "height", value = "Block height", required = true, dataType = "integer", paramType = "path")
    ))
  def at: Route = (path("at" / IntNumber) & get)(at(_, includeTransactions = true))

  @Path("/headers/at/{height}")
  @ApiOperation(value = "Block header at height", notes = "Get block header at specified height", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "height", value = "Block height", required = true, dataType = "integer", paramType = "path")
    ))
  def atHeaderOnly: Route = (path("headers" / "at" / IntNumber) & get)(at(_, includeTransactions = false))

  private def at(height: Int, includeTransactions: Boolean): StandardRoute = {

    (if (includeTransactions) {
       commonApi.blockAtHeight(height).map(_.json())
     } else {
       commonApi.blockHeaderAtHeight(height).map { case (bh, s) => BlockHeader.json(bh, s) }
     }) match {
      case Some(json) => complete(json.addBlockFields(height))
      case None       => complete(Json.obj("status" -> "error", "details" -> "No block for this height"))
    }
  }

  @Path("/seq/{from}/{to}")
  @ApiOperation(value = "Block range", notes = "Get blocks at specified heights", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "from", value = "Start block height", required = true, dataType = "integer", paramType = "path"),
      new ApiImplicitParam(name = "to", value = "End block height", required = true, dataType = "integer", paramType = "path")
    ))
  def seq: Route = (path("seq" / IntNumber / IntNumber) & get) { (start, end) =>
    seq(start, end, includeTransactions = true)
  }

  @Path("/headers/seq/{from}/{to}")
  @ApiOperation(value = "Block header range", notes = "Get block headers at specified heights", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "from", value = "Start block height", required = true, dataType = "integer", paramType = "path"),
      new ApiImplicitParam(name = "to", value = "End block height", required = true, dataType = "integer", paramType = "path")
    ))
  def seqHeaderOnly: Route = (path("headers" / "seq" / IntNumber / IntNumber) & get) { (start, end) =>
    seq(start, end, includeTransactions = false)
  }

  private def seq(start: Int, end: Int, includeTransactions: Boolean): Route = {
    if (end >= 0 && start >= 0 && end - start >= 0 && end - start < MaxBlocksPerRequest) {
      val blocks = if (includeTransactions) {
        commonApi
          .blocksRange(start, end)
          .map(bh => bh._1.json().addBlockFields(bh._2))
      } else {
        commonApi
          .blockHeadersRange(start, end)
          .map { case (bh, size, height) => BlockHeader.json(bh, size).addBlockFields(height) }
      }

      extractScheduler(implicit sc => complete(blocks.toListL.map(JsArray(_)).runToFuture))
    } else {
      complete(TooBigArrayAllocation)
    }
  }

  @Path("/last")
  @ApiOperation(value = "Last block", notes = "Get last block", httpMethod = "GET")
  def last: Route = (path("last") & get)(last(includeTransactions = true))

  @Path("/headers/last")
  @ApiOperation(value = "Last block header", notes = "Get last block header", httpMethod = "GET")
  def lastHeaderOnly: Route = (path("headers" / "last") & get)(last(includeTransactions = false))

  def last(includeTransactions: Boolean): StandardRoute = {
    complete {
      val height = blockchain.height
      (if (includeTransactions) {
         commonApi.lastBlock().map(_.json())
       } else {
         commonApi.lastBlock().map(block => BlockHeader.json(block, block.bytes().length))
       }).map(_.addBlockFields(height))
    }
  }

  @Path("/first")
  @ApiOperation(value = "Genesis block", notes = "Get genesis block", httpMethod = "GET")
  def first: Route = (path("first") & get) {
    complete(commonApi.firstBlock().json().addBlockFields(1))
  }

  @Path("/signature/{signature}")
  @ApiOperation(value = "Block by signature", notes = "Get block by its signature", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "signature", value = "Base58-encoded block signature", required = true, dataType = "string", paramType = "path")
    ))
  def signature: Route = (path("signature" / Segment) & get) { encodedSignature =>
    if (encodedSignature.length > TransactionParsers.SignatureStringLength) {
      complete(InvalidSignature)
    } else {
      val result = for {
        blockId <- ByteStr
          .decodeBase58(encodedSignature)
          .toOption
          .toRight(InvalidSignature)

        block <- commonApi.blockBySignature(blockId).toRight(BlockDoesNotExist)
      } yield block.json().addBlockFields(block.uniqueId)

      complete(result)
    }
  }

  private[this] implicit class JsonObjectOps(json: JsObject) {
    def addBlockFields(blockId: ByteStr): JsObject =
      json ++ blockchain
        .heightOf(blockId)
        .map(height => Json.obj("height" -> height, "totalFee" -> blockchain.totalFee(height).fold(JsNull: JsValue)(JsNumber(_))))
        .getOrElse(JsObject.empty)

    def addBlockFields(height: Int): JsObject =
      json ++ Json.obj("height" -> height, "totalFee" -> blockchain.totalFee(height).fold(JsNull: JsValue)(JsNumber(_)))
  }
}
