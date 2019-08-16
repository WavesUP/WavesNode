package com.wavesplatform.api.http.alias

import akka.http.scaladsl.server.Route
import com.wavesplatform.account.Alias
import com.wavesplatform.api.http.ApiError.AliasDoesNotExist
import com.wavesplatform.api.http._
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction._
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json.{Format, Json}

@Path("/alias")
@Api(value = "/alias")
case class AliasApiRoute(settings: RestAPISettings, wallet: Wallet, utxPoolSynchronizer: UtxPoolSynchronizer, time: Time, blockchain: Blockchain)
    extends BroadcastRoute
    with AuthRoute {

  override val route = pathPrefix("alias") {
    addressOfAlias ~ aliasOfAddress ~ deprecatedRoute
  }

  private def deprecatedRoute: Route =
    path("broadcast" / "create") {
      broadcast[SignedCreateAliasV1Request](_.toTx)
    } ~ (path("create") & withAuth) {
      broadcast[CreateAliasV1Request](TransactionFactory.aliasV1(_, wallet, time))
    }

  @Path("/by-alias/{alias}")
  @ApiOperation(
    value = "Address by alias",
    notes = "Returns an address associated with an Alias. Alias should be plain text without an 'alias' prefix and network code.",
    httpMethod = "GET"
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "alias", value = "Alias", required = true, dataType = "string", paramType = "path")
    )
  )
  def addressOfAlias: Route = (get & path("by-alias" / Segment)) { aliasName =>
    val result = Alias.create(aliasName) match {
      case Right(alias) =>
        blockchain.resolveAlias(alias) match {
          case Right(addr) => Right(Address(addr.stringRepr))
          case _           => Left(AliasDoesNotExist(alias))
        }
      case Left(err) => Left(ApiError.fromValidationError(err))
    }
    complete(result)
  }

  @Path("/by-address/{address}")
  @ApiOperation(value = "Aliases by address", notes = "Returns a collection of aliases associated with an address", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    )
  )
  def aliasOfAddress: Route = (get & path("by-address" / Segment)) { addressString =>
    val result: Either[ApiError, Seq[String]] = com.wavesplatform.account.Address
      .fromString(addressString)
      .map(acc => blockchain.aliasesOfAddress(acc).map(_.stringRepr).toVector)
      .left
      .map(ApiError.fromValidationError)
    complete(result)
  }

  case class Address(address: String)

  implicit val addressFormat: Format[Address] = Json.format
}
