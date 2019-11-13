package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.utils._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.transaction.assets.IssueTransaction
import play.api.libs.json.Json

case class IssueRequest(
    version: Option[Byte],
    sender: Option[String],
    senderPublicKey: Option[String],
    name: String,
    description: String,
    quantity: Long,
    decimals: Byte,
    reissuable: Boolean,
    script: Option[String],
    fee: Long,
    timestamp: Option[Long],
    signature: Option[String],
    proofs: Option[List[String]]
) extends TxBroadcastRequest {
  def toTxFrom(sender: PublicKey): Either[ValidationError, IssueTransaction] =
    for {
      validProofs <- toProofs(version, signature, proofs)
      tx <- IssueTransaction.create(
        version.getOrElse(defaultVersion),
        sender,
        name.utf8Bytes,
        description.utf8Bytes,
        quantity,
        decimals,
        reissuable,
        script.map(str => Script.fromBase64String(str).explicitGet()),
        fee,
        timestamp.getOrElse(defaultTimestamp),
        validProofs
      )
    } yield tx
}

object IssueRequest {
  implicit val jsonFormat = Json.format[IssueRequest]
}
