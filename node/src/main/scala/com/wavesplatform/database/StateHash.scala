package com.wavesplatform.database

import com.google.common.io.ByteStreams
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.crypto
import com.wavesplatform.state.Diff
import com.wavesplatform.transaction.Asset
import play.api.libs.json.{Format, JsString, Json, OFormat}
import scorex.utils.ByteArray

import scala.collection.mutable

case class StateHash(stateHash: Array[Byte], diffHash: Array[Byte], snapshotHash: Array[Byte])

object StateHash {
  private def toBytes(asset: Asset) = asset match {
    case Asset.Waves           => Array.emptyByteArray
    case Asset.IssuedAsset(id) => id.arr
  }

  private implicit val byteArrayOrdering: Ordering[Array[Byte]] = ByteArray.compare
  private implicit val assetOrdering: Ordering[Asset]           = Ordering.by(toBytes)
  private implicit val address: Ordering[Address]               = Ordering.by(_.bytes.arr)

  implicit val byteArrayFormat: Format[Array[Byte]] = Format(
    _.validate[String].map(Base64.decode),
    v => JsString(Base64.encode(v))
  )

  implicit val format: OFormat[StateHash] = Json.format

  def hash(balances: Seq[(Address, Asset, Long)]): Array[Byte] = {
    val baos = ByteStreams.newDataOutput()
    for ((address, asset, balance) <- balances.sorted) {
      baos.write(address.bytes.arr)
      baos.write(toBytes(asset))
      baos.writeLong(balance)
    }

    crypto.fastHash(baos.toByteArray)
  }

  def hash(diff: Diff): Array[Byte] = {
    val buf = mutable.ArrayBuffer.empty[(Address, Asset, Long)]

    for ((address, portfolio) <- diff.portfolios) {
      buf += ((address, Asset.Waves, portfolio.balance))
      buf ++= (for {
        (assetId, balance) <- portfolio.assets
      } yield (address, assetId, balance))
    }

    hash(buf)
  }

  def hash(prevStateHash: Array[Byte], diffHash: Array[Byte], snapshotHash: Array[Byte]): Array[Byte] =
    crypto.fastHash(prevStateHash ++ diffHash ++ snapshotHash)

}
