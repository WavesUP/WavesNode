package com.wavesplatform.state

import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{Asset, Transaction}
import play.api.libs.json._

case class LeaseBalance(in: Long, out: Long)

object LeaseBalance {
  val empty = LeaseBalance(0, 0)

  implicit val m: Monoid[LeaseBalance] = new Monoid[LeaseBalance] {
    override def empty: LeaseBalance = LeaseBalance.empty

    override def combine(x: LeaseBalance, y: LeaseBalance): LeaseBalance =
      LeaseBalance(safeSum(x.in, y.in), safeSum(x.out, y.out))
  }

  implicit val leaseBalanceJsonFormat: Format[LeaseBalance] = Json.format
}

case class VolumeAndFee(volume: Long, fee: Long)

object VolumeAndFee {
  val empty = VolumeAndFee(0, 0)

  implicit val m: Monoid[VolumeAndFee] = new Monoid[VolumeAndFee] {
    override def empty: VolumeAndFee = VolumeAndFee.empty

    override def combine(x: VolumeAndFee, y: VolumeAndFee): VolumeAndFee =
      VolumeAndFee(x.volume + y.volume, x.fee + y.fee)
  }
}

case class AssetInfo(name: String, description: String)

case class AssetDetails(isReissuable: Boolean, volume: BigInt)
object AssetDetails {
  implicit val assetInfoMonoid: Monoid[AssetDetails] = new Monoid[AssetDetails] {
    override def empty: AssetDetails = AssetDetails(isReissuable = true, 0)
    override def combine(x: AssetDetails, y: AssetDetails): AssetDetails =
      AssetDetails(x.isReissuable && y.isReissuable, x.volume + y.volume)
  }
}

case class AssetDescription(
    issuer: PublicKey,
    name: String,
    description: String,
    decimals: Int,
    reissuable: Boolean,
    totalVolume: BigInt,
    lastInfoUpdateHeight: Height,
    script: Option[Script],
    sponsorship: Long
) {
  override def equals(obj: scala.Any) = obj match {
    case o: AssetDescription =>
      o.issuer == this.issuer &&
        o.name.equals(name) &&
        o.description.equals(description) &&
        o.decimals == decimals &&
        o.reissuable == reissuable &&
        o.totalVolume == totalVolume &&
        o.script == script &&
        o.sponsorship == sponsorship
    case _ => false
  }
}

case class AccountDataInfo(data: Map[String, DataEntry[_]])

object AccountDataInfo {
  implicit val accountDataInfoMonoid: Monoid[AccountDataInfo] = new Monoid[AccountDataInfo] {
    override def empty: AccountDataInfo = AccountDataInfo(Map.empty)

    override def combine(x: AccountDataInfo, y: AccountDataInfo): AccountDataInfo = AccountDataInfo(x.data ++ y.data)
  }
}

sealed abstract class Sponsorship
case class SponsorshipValue(minFee: Long) extends Sponsorship
case object SponsorshipNoInfo             extends Sponsorship

object Sponsorship {
  implicit val sponsorshipMonoid: Monoid[Sponsorship] = new Monoid[Sponsorship] {
    override def empty: Sponsorship = SponsorshipNoInfo

    override def combine(x: Sponsorship, y: Sponsorship): Sponsorship = y match {
      case SponsorshipNoInfo => x
      case _                 => y
    }
  }

  def calcWavesFeeAmount(tx: Transaction, getSponsorship: IssuedAsset => Option[Long]): Long = tx.assetFee match {
    case (asset @ IssuedAsset(_), amountInAsset) =>
      val sponsorship = getSponsorship(asset).getOrElse(0L)
      Sponsorship.toWaves(amountInAsset, sponsorship)

    case (Asset.Waves, amountInWaves) =>
      amountInWaves
  }

  def sponsoredFeesSwitchHeight(blockchain: Blockchain): Int =
    blockchain
      .featureActivationHeight(BlockchainFeatures.FeeSponsorship.id)
      .map(h => h + blockchain.settings.functionalitySettings.activationWindowSize(h))
      .getOrElse(Int.MaxValue)

  def toWaves(assetFee: Long, sponsorship: Long): Long = {
    if (sponsorship == 0) return Long.MaxValue
    val waves = BigInt(assetFee) * FeeValidation.FeeUnit / sponsorship
    waves.bigInteger.longValueExact()
  }

  def fromWaves(wavesFee: Long, sponsorship: Long): Long = {
    if (wavesFee == 0 || sponsorship == 0) return 0
    val assetFee = BigInt(wavesFee) * sponsorship / FeeValidation.FeeUnit
    assetFee.bigInteger.longValueExact()
  }
}

case class Diff(
                 transactions: Map[ByteStr, (Transaction, Set[Address])],
                 portfolios: Map[Address, Portfolio],
                 issuedAssets: Map[IssuedAsset, AssetDetails],
                 updatedAssets: Map[IssuedAsset, AssetInfo],
                 aliases: Map[Alias, Address],
                 orderFills: Map[ByteStr, VolumeAndFee],
                 leaseState: Map[ByteStr, Boolean],
                 scripts: Map[Address, Option[(Script, Long)]],
                 assetScripts: Map[IssuedAsset, Option[(Script, Long)]],
                 accountData: Map[Address, AccountDataInfo],
                 sponsorship: Map[IssuedAsset, Sponsorship],
                 scriptsRun: Int,
                 scriptsComplexity: Long,
                 scriptResults: Map[ByteStr, InvokeScriptResult]
)

object Diff {
  def stateOps(
                portfolios: Map[Address, Portfolio] = Map.empty,
                assetDetails: Map[IssuedAsset, AssetDetails] = Map.empty,
                assetInfos: Map[IssuedAsset, AssetInfo] = Map.empty,
                aliases: Map[Alias, Address] = Map.empty,
                orderFills: Map[ByteStr, VolumeAndFee] = Map.empty,
                leaseState: Map[ByteStr, Boolean] = Map.empty,
                scripts: Map[Address, Option[(Script, Long)]] = Map.empty,
                assetScripts: Map[IssuedAsset, Option[(Script, Long)]] = Map.empty,
                accountData: Map[Address, AccountDataInfo] = Map.empty,
                sponsorship: Map[IssuedAsset, Sponsorship] = Map.empty,
                scriptResults: Map[ByteStr, InvokeScriptResult] = Map.empty
  ): Diff =
    Diff(
      transactions = Map(),
      portfolios = portfolios,
      issuedAssets = assetDetails,
      updatedAssets = assetInfos,
      aliases = aliases,
      orderFills = orderFills,
      leaseState = leaseState,
      scripts = scripts,
      assetScripts = assetScripts,
      accountData = accountData,
      sponsorship = sponsorship,
      scriptsRun = 0,
      scriptResults = scriptResults,
      scriptsComplexity = 0
    )

  def apply(
             tx: Transaction,
             portfolios: Map[Address, Portfolio] = Map.empty,
             assetDetails: Map[IssuedAsset, AssetDetails] = Map.empty,
             assetInfos: Map[IssuedAsset, AssetInfo] = Map.empty,
             aliases: Map[Alias, Address] = Map.empty,
             orderFills: Map[ByteStr, VolumeAndFee] = Map.empty,
             leaseState: Map[ByteStr, Boolean] = Map.empty,
             scripts: Map[Address, Option[(Script, Long)]] = Map.empty,
             assetScripts: Map[IssuedAsset, Option[(Script, Long)]] = Map.empty,
             accountData: Map[Address, AccountDataInfo] = Map.empty,
             sponsorship: Map[IssuedAsset, Sponsorship] = Map.empty,
             scriptsRun: Int = 0,
             scriptsComplexity: Long = 0,
             scriptResults: Map[ByteStr, InvokeScriptResult] = Map.empty
  ): Diff =
    Diff(
      transactions = Map((tx.id(), (tx, (portfolios.keys ++ accountData.keys).toSet))),
      portfolios = portfolios,
      issuedAssets = assetDetails,
      updatedAssets = assetInfos,
      aliases = aliases,
      orderFills = orderFills,
      leaseState = leaseState,
      scripts = scripts,
      assetScripts = assetScripts,
      accountData = accountData,
      sponsorship = sponsorship,
      scriptsRun = scriptsRun,
      scriptResults = scriptResults,
      scriptsComplexity = scriptsComplexity
    )

  val empty = new Diff(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, 0, 0, Map.empty)

  implicit val diffMonoid: Monoid[Diff] = new Monoid[Diff] {
    override def empty: Diff = Diff.empty

    override def combine(older: Diff, newer: Diff): Diff =
      Diff(
        transactions = older.transactions ++ newer.transactions,
        portfolios = older.portfolios.combine(newer.portfolios),
        issuedAssets = older.issuedAssets.combine(newer.issuedAssets),
        updatedAssets = older.updatedAssets ++ newer.updatedAssets,
        aliases = older.aliases ++ newer.aliases,
        orderFills = older.orderFills.combine(newer.orderFills),
        leaseState = older.leaseState ++ newer.leaseState,
        scripts = older.scripts ++ newer.scripts,
        assetScripts = older.assetScripts ++ newer.assetScripts,
        accountData = older.accountData.combine(newer.accountData),
        sponsorship = older.sponsorship.combine(newer.sponsorship),
        scriptsRun = older.scriptsRun.combine(newer.scriptsRun),
        scriptResults = older.scriptResults.combine(newer.scriptResults),
        scriptsComplexity = older.scriptsComplexity + newer.scriptsComplexity
      )
  }
}
