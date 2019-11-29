package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.DataEntry
import com.wavesplatform.transaction.{DataTransaction, Proofs}
import play.api.libs.json.{Format, Json}

object DataRequest {
  implicit val unsignedDataRequestReads: Format[DataRequest] = Json.format
}

case class DataRequest(sender: String, data: List[DataEntry[_]], fee: Long, timestamp: Option[Long] = None)

case class SignedDataRequest(senderPublicKey: String, data: List[DataEntry[_]], fee: Long, timestamp: Long, proofs: Proofs) {
  def toTx: Either[ValidationError, DataTransaction] =
    for {
      _sender <- PublicKey.fromBase58String(senderPublicKey)
      t       <- DataTransaction.create(1.toByte, _sender, data, fee, timestamp, proofs)
    } yield t
}

object SignedDataRequest {
  implicit val signedDataRequestReads: Format[SignedDataRequest] = Json.format
}
