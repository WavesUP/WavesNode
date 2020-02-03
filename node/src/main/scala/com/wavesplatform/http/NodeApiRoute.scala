package com.wavesplatform.http

import java.time.Instant

import akka.http.scaladsl.server.Route
import com.wavesplatform.Shutdownable
import com.wavesplatform.api.http.swagger.SwaggerDocService.ApiKeyDefName
import com.wavesplatform.api.http.{ApiRoute, AuthRoute}
import com.wavesplatform.settings.{Constants, RestAPISettings}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.utils.ScorexLogging
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json.Json

@Path("/node")
@Api(value = "node")
case class NodeApiRoute(settings: RestAPISettings, blockchain: Blockchain, application: Shutdownable)
    extends ApiRoute
    with AuthRoute
    with ScorexLogging {

  override lazy val route = pathPrefix("node") {
    stop ~ status ~ version
  }

  @Path("/version")
  @ApiOperation(value = "Version", notes = "Get Waves node version", httpMethod = "GET", response = classOf[VersionDesc])
  def version: Route = (get & path("version")) {
    complete(Json.obj("version" -> Constants.AgentName))
  }

  @Path("/stop")
  @ApiOperation(
    value = "Stop",
    notes = "Stop the node",
    httpMethod = "POST",
    authorizations = Array(new Authorization(ApiKeyDefName)),
    response = classOf[StoppedDesc]
  )
  def stop: Route = (post & path("stop") & withAuth) {
    log.info("Request to stop application")
    application.shutdown()
    complete(Json.obj("stopped" -> true))
  }

  @Path("/status")
  @ApiOperation(value = "Status", notes = "Get status of the running core", httpMethod = "GET", response = classOf[StatusDesc])
  def status: Route = (get & path("status")) {
    val lastUpdated = blockchain.lastBlock.get.timestamp
    complete(
      Json.obj(
        "blockchainHeight" -> blockchain.height,
        "stateHeight"      -> blockchain.height,
        "updatedTimestamp" -> lastUpdated,
        "updatedDate"      -> Instant.ofEpochMilli(lastUpdated).toString
      )
    )
  }

  private[this] case class VersionDesc(version: String)
  private[this] case class StoppedDesc(stopped: Boolean)
  private[this] case class StatusDesc(blockchainHeight: Int, stateHeight: Int, updatedTimestamp: Long, updatedDate: String)
}
