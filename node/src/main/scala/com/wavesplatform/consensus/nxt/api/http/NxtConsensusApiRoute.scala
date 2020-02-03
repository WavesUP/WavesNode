package com.wavesplatform.consensus.nxt.api.http

import akka.http.scaladsl.server.Route
import com.wavesplatform.account.Address
import com.wavesplatform.api.http.{ApiError, ApiRoute, CommonApiFunctions}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json.Json

@Path("/consensus")
@Api(value = "/consensus")
case class NxtConsensusApiRoute(settings: RestAPISettings, blockchain: Blockchain) extends ApiRoute with CommonApiFunctions {
  override val route: Route =
    pathPrefix("consensus") {
      algo ~ basetarget ~ baseTargetId ~ generationSignature ~ generationSignatureId ~ generatingBalance
    }

  @Path("/generatingbalance/{address}")
  @ApiOperation(
    value = "Generating balance",
    notes = "Account's generating balance(the same as balance atm)",
    httpMethod = "GET",
    response = classOf[GeneratingBalanceDesc]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    )
  )
  def generatingBalance: Route = (path("generatingbalance" / Segment) & get) { address =>
    Address.fromString(address) match {
      case Left(_) => complete(ApiError.InvalidAddress)
      case Right(account) =>
        complete(Json.obj("address" -> account.stringRepr, "balance" -> blockchain.generatingBalance(account)))
    }
  }

  @Path("/generationsignature/{blockId}")
  @ApiOperation(
    value = "Generation signature",
    notes = "Generation signature of a block with specified id",
    httpMethod = "GET",
    response = classOf[GenerationSignatureDesc]
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "blockId", value = "Block id", required = true, dataType = "string", paramType = "path")
    )
  )
  def generationSignatureId: Route = (path("generationsignature" / Segment) & get) { encodedSignature =>
    withBlock(blockchain, encodedSignature) { block =>
      complete(Json.obj("generationSignature" -> block.consensusData.generationSignature.base58))
    }
  }

  @Path("/generationsignature")
  @ApiOperation(
    value = "Generation signature last",
    notes = "Generation signature of a last block",
    httpMethod = "GET",
    response = classOf[GenerationSignatureDesc]
  )
  def generationSignature: Route = (path("generationsignature") & get) {
    complete(Json.obj("generationSignature" -> blockchain.lastBlock.get.consensusData.generationSignature.base58))
  }

  @Path("/basetarget/{blockId}")
  @ApiOperation(value = "Base target", notes = "base target of a block with specified id", httpMethod = "GET", response = classOf[BaseTargetDesc])
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "blockId", value = "Block id", required = true, dataType = "string", paramType = "path")
    )
  )
  def baseTargetId: Route = (path("basetarget" / Segment) & get) { encodedSignature =>
    withBlock(blockchain, encodedSignature) { block =>
      complete(Json.obj("baseTarget" -> block.consensusData.baseTarget))
    }
  }

  @Path("/basetarget")
  @ApiOperation(value = "Base target last", notes = "Base target of a last block", httpMethod = "GET", response = classOf[BaseTargetWithScoreDesc])
  def basetarget: Route = (path("basetarget") & get) {
    complete(
      Json.obj(
        "baseTarget" -> blockchain.lastBlock.get.consensusData.baseTarget,
        "score"      -> blockchain.score.toString()
      )
    )
  }

  @Path("/algo")
  @ApiOperation(value = "Consensus algo", notes = "Shows which consensus algo being using", httpMethod = "GET", response = classOf[AlgoDesc])
  def algo: Route = (path("algo") & get) {
    complete(
      if (blockchain.activatedFeatures.contains(BlockchainFeatures.FairPoS.id))
        Json.obj("consensusAlgo" -> "Fair Proof-of-Stake (FairPoS)")
      else
        Json.obj("consensusAlgo" -> "proof-of-stake (PoS)")
    )
  }

  private[this] case class AlgoDesc(consensusAlgo: String)
  private[this] case class BaseTargetDesc(baseTarget: Long)
  private[this] case class BaseTargetWithScoreDesc(baseTarget: Long, score: String)
  private[this] case class GenerationSignatureDesc(generationSignature: String)
  private[this] case class GeneratingBalanceDesc(address: String, balance: Long)
}
