package com.wavesplatform.transaction.serialization.impl

import com.wavesplatform.common.utils.Base58
import com.wavesplatform.transaction.{ProvenTransaction, SigProofsSwitch}
import play.api.libs.json.{JsArray, JsObject, JsString, Json}

object ProvenTxJson {
  def toJson(tx: ProvenTransaction): JsObject = {
    import tx._
    Json.obj(
      "type"            -> typeId,
      "id"              -> id().toString,
      "sender"          -> sender.stringRepr,
      "senderPublicKey" -> Base58.encode(sender),
      "fee"             -> assetFee._2,
      "feeAssetId"      -> assetFee._1.maybeBase58Repr,
      "timestamp"       -> timestamp,
      "proofs"          -> JsArray(proofs.proofs.map(p => JsString(p.toString)))
    ) ++ (tx match {
      // Compatibility
      case s: SigProofsSwitch if s.usesLegacySignature => Json.obj("signature" -> tx.signature.toString)
      case _                                           => Json.obj()
    })
  }
}
