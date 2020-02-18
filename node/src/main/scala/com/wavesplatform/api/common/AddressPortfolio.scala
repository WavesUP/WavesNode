package com.wavesplatform.api.common

import cats.instances.map._
import cats.syntax.option._
import cats.syntax.semigroup._
import com.google.common.collect.AbstractIterator
import com.google.common.primitives.Shorts
import com.wavesplatform.account.Address
import com.wavesplatform.crypto
import com.wavesplatform.database.{DBResource, Keys, readIntSeq}
import com.wavesplatform.state.Portfolio.longSemigroup
import com.wavesplatform.state.{AssetDescription, Diff}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.utils.ScorexLogging

import scala.collection.JavaConverters._

object AddressPortfolio extends ScorexLogging {
  def nftIterator(
      resource: DBResource,
      address: Address,
      diff: Diff,
      maybeAfter: Option[IssuedAsset],
      loadAssetDescription: IssuedAsset => Option[AssetDescription]
  ): Iterator[(IssuedAsset, AssetDescription)] = resource.get(Keys.addressId(address)).fold(Iterator[(IssuedAsset, AssetDescription)]()) {
    addressId =>
      val keyPrefixBytes = Shorts.toByteArray(Keys.AssetBalanceHistoryPrefix) ++ addressId.toByteArray
      resource.iterator.seek(keyPrefixBytes ++ maybeAfter.fold(Array.emptyByteArray)(_.id.arr))

      for (after <- maybeAfter) {
        @inline
        def skipEntry(key: Array[Byte]): Boolean = {
          log.info(s"LOAD NFT: key=${key.mkString(",")}, prefix=${keyPrefixBytes.mkString(",")}, suffix=${after.id.arr.mkString(",")}")
          key.startsWith(keyPrefixBytes) && !key.endsWith(after.id.arr)
        }

        while (resource.iterator.hasNext && skipEntry(resource.iterator.next().getKey)) {}
        log.info(s"LOAD NFT: skipping one more entry")
        resource.iterator.next()
      }

      log.info(s"LOAD NFT: assets=${diff.portfolios.get(address).map(_.assets).orEmpty}")

      new BalanceIterator(Some(addressId), _ => true, resource, diff.portfolios.get(address).map(_.assets).orEmpty).asScala
        .collect { case (issuedAsset, balance) if balance != 0 => issuedAsset -> loadAssetDescription(issuedAsset) }
        .collect { case (id, Some(ad)) if ad.nft => id -> ad }
  }

  def assetBalanceIterator(
      resource: DBResource,
      address: Address,
      diff: Diff,
      includeAsset: IssuedAsset => Boolean
  ): Iterator[(IssuedAsset, Long)] = {
    val maybeAddressId = resource.get(Keys.addressId(address))
    maybeAddressId.foreach { addressId =>
      resource.iterator.seek(Shorts.toByteArray(Keys.AssetBalanceHistoryPrefix) ++ addressId.toByteArray)
    }
    new BalanceIterator(maybeAddressId, includeAsset, resource, diff.portfolios.get(address).map(_.assets).orEmpty).asScala
      .filter(_._2 != 0)
  }

  class BalanceIterator(
      addressId: Option[BigInt],
      includeAsset: IssuedAsset => Boolean,
      resource: DBResource,
      private var pendingOverrides: Map[IssuedAsset, Long]
  ) extends AbstractIterator[(IssuedAsset, Long)] {

    private val prefix = addressId.map(aid => Shorts.toByteArray(Keys.AssetBalanceHistoryPrefix) ++ aid.toByteArray)
    private def stillSameAddress(k: Array[Byte]): Boolean = prefix.exists { p =>
      k.length == (p.length + crypto.DigestLength) && k.startsWith(p)
    }

    private def loadBalance(assetId: IssuedAsset, history: Seq[Int]): Long = {
      if (!includeAsset(assetId)) 0
      else {
        val balanceFromDiff = pendingOverrides.getOrElse(assetId, 0L)
        val balanceFromHistory = (for {
          id <- addressId
          h  <- history.headOption
        } yield resource.get(Keys.assetBalance(id, assetId)(h))).getOrElse(0L)
        balanceFromDiff |+| balanceFromHistory
      }
    }

    override def computeNext(): (IssuedAsset, Long) =
      if (resource.iterator.hasNext && stillSameAddress(resource.iterator.peekNext().getKey)) {
        val currentEntry = resource.iterator.next()
        val assetId      = IssuedAsset(currentEntry.getKey.takeRight(crypto.DigestLength))
        val balance      = loadBalance(assetId, readIntSeq(currentEntry.getValue))
        pendingOverrides -= assetId
        assetId -> balance
      } else if (pendingOverrides.nonEmpty) {
        val (asset, balance) = pendingOverrides.head
        val bool             = includeAsset(asset)
        val l                = if (bool) balance else 0
        pendingOverrides -= asset
        asset -> l
      } else endOfData()
  }
}
