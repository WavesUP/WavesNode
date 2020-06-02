package com.wavesplatform.api.http.requests

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import play.api.libs.json.Json

case class MassTransferRequest(
    version: Option[Byte],
    assetId: Option[String],
    sender: String,
    transfers: List[Transfer],
    fee: Long,
    attachment: ByteStr,
    timestamp: Option[Long] = None
)

object MassTransferRequest {
  implicit val jsonFormat = Json.format[MassTransferRequest]
}
