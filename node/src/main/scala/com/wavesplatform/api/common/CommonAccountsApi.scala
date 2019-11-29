package com.wavesplatform.api.common

import com.google.common.base.Charsets
import com.google.common.primitives.Shorts
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.common
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{DBExt, Keys}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.{AccountScriptInfo, Blockchain, DataEntry, Diff, Height}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.utils.ScorexLogging
import monix.reactive.Observable
import org.iq80.leveldb.DB

import scala.collection.mutable

trait CommonAccountsApi {
  import CommonAccountsApi._

  def balance(address: Address, confirmations: Int = 0): Long

  def effectiveBalance(address: Address, confirmations: Int = 0): Long

  def balanceDetails(address: Address): BalanceDetails

  def assetBalance(address: Address, asset: IssuedAsset): Long

  def portfolio(address: Address): Observable[(IssuedAsset, Long)]

  def nftPortfolio(address: Address, from: Option[IssuedAsset]): Observable[IssueTransaction]

  def script(address: Address): Option[AccountScriptInfo]

  def data(address: Address, key: String): Option[DataEntry[_]]

  def dataStream(address: Address, regex: Option[String]): Observable[DataEntry[_]]

  def activeLeases(address: Address): Observable[(Height, LeaseTransaction)]

  def resolveAlias(alias: Alias): Either[ValidationError, Address]
}

object CommonAccountsApi extends ScorexLogging {
  final case class BalanceDetails(regular: Long, generating: Long, available: Long, effective: Long, leaseIn: Long, leaseOut: Long)

  def apply(diff: => Diff, db: DB, blockchain: Blockchain): CommonAccountsApi = new CommonAccountsApi {

    override def balance(address: Address, confirmations: Int = 0): Long = {
      blockchain.balance(address, blockchain.height, confirmations)
    }

    override def effectiveBalance(address: Address, confirmations: Int = 0): Long = {
      blockchain.effectiveBalance(address, confirmations)
    }

    override def balanceDetails(address: Address): BalanceDetails = {
      val portfolio = blockchain.wavesPortfolio(address)
      BalanceDetails(
        portfolio.balance,
        blockchain.generatingBalance(address),
        portfolio.balance - portfolio.lease.out,
        portfolio.effectiveBalance,
        portfolio.lease.in,
        portfolio.lease.out
      )
    }

    override def assetBalance(address: Address, asset: IssuedAsset): Long = blockchain.balance(address, asset)

    override def portfolio(address: Address): Observable[(IssuedAsset, Long)] =
      common.portfolio(
        db,
        address,
        diff.portfolios.get(address).fold(Map.empty[IssuedAsset, Long])(_.assets),
        assetId => !blockchain.isFeatureActivated(BlockchainFeatures.ReduceNFTFee) || !blockchain.assetDescription(assetId).exists(_.isNFT)
      )

    override def nftPortfolio(address: Address, from: Option[IssuedAsset]): Observable[IssueTransaction] = {
      log.info(s"Diff: $diff")
      nftList(
        db,
        address,
        diff,
        id => blockchain.assetDescription(id).exists(_.isNFT),
        from
      )
    }

    override def script(address: Address): Option[AccountScriptInfo] = blockchain.accountScript(address)

    override def data(address: Address, key: String): Option[DataEntry[_]] =
      blockchain.accountData(address, key)

    override def dataStream(address: Address, regex: Option[String]): Observable[DataEntry[_]] = {
      val entriesFromDiff = diff.accountData.get(address).fold[Map[String, DataEntry[_]]](Map.empty)(_.data)
      val entries         = mutable.ArrayBuffer[DataEntry[_]](entriesFromDiff.values.toSeq: _*)

      db.readOnly { ro =>
        val addressId = db.get(Keys.addressId(address)).get
        db.iterateOver(Shorts.toByteArray(Keys.DataHistoryPrefix) ++ addressId.toByteArray) { e =>
          val key = new String(e.getKey.drop(2 + addressId.toByteArray.length), Charsets.UTF_8)
          if (regex.forall(_.r.pattern.matcher(key).matches()) && !entriesFromDiff.contains(key)) {
            for (h <- ro.get(Keys.dataHistory(addressId, key)).headOption; e <- ro.get(Keys.data(addressId, key)(h))) {
              entries += e
            }
          }
        }
      }
      Observable.fromIterable(entries)
    }

    override def resolveAlias(alias: Alias): Either[ValidationError, Address] = blockchain.resolveAlias(alias)

    override def activeLeases(address: Address): Observable[(Height, LeaseTransaction)] = {
      def leaseIsActive(id: ByteStr): Boolean = {
        val leaseDetails = blockchain.leaseDetails(id)
        val active       = leaseDetails.exists(_.isActive)
        log.info(s"Lease $id: $leaseDetails, active=$active")
        active
      }
      common.activeLeases(db, Some(Height(blockchain.height) -> diff), address, leaseIsActive)
    }
  }
}