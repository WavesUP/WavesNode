package com.wavesplatform.state.diffs

import cats.implicits._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.traits.domain.{Burn, Reissue}
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets._

object AssetTransactionsDiff {
  def issue(blockchain: Blockchain)(tx: IssueTransaction): Either[ValidationError, Diff] = {
    val info  = AssetInfo(isReissuable = tx.reissuable, volume = tx.quantity)
    val asset = IssuedAsset(tx.id())
    DiffsCommon.countScriptComplexity(tx.script, blockchain)
      .map(script =>
        Diff(
          tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map(asset -> tx.quantity))),
        assetInfos = Map(asset               -> info),
        assetScripts = Map(asset -> script),
          scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)
        )
      )
  }

  def setAssetScript(blockchain: Blockchain, blockTime: Long)(tx: SetAssetScriptTransaction): Either[ValidationError, Diff] =
    DiffsCommon.validateAsset(blockchain, tx.asset, tx.sender, issuerOnly = true).flatMap { _ =>
      if (blockchain.hasAssetScript(tx.asset)) {
        DiffsCommon.countScriptComplexity(tx.script, blockchain)
          .map(script =>
            Diff(
              tx = tx,
            portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map.empty)),
            assetScripts = Map(tx.asset          -> script),
              scriptsRun =
                // Asset script doesn't count before Ride4DApps activation
                if (blockchain.isFeatureActivated(BlockchainFeatures.Ride4DApps, blockchain.height)) {
                  DiffsCommon.countScriptRuns(blockchain, tx)
                } else {
                  Some(tx.sender.toAddress).count(blockchain.hasScript)
                }
            )
          )
      } else {
        Left(GenericError("Cannot set script on an asset issued without a script"))
      }
    }

  def reissue(blockchain: Blockchain, blockTime: Long)(tx: ReissueTransaction): Either[ValidationError, Diff] =
    DiffsCommon.processReissue(
      blockchain,
      tx.sender,
      blockTime,
      tx.fee,
      Reissue(tx.asset.id, tx.reissuable, tx.quantity)
    ).map(Diff(tx = tx, scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)) |+| _)

  def burn(blockchain: Blockchain)(tx: BurnTransaction): Either[ValidationError, Diff] =
    DiffsCommon.processBurn(
      blockchain,
      tx.sender,
      tx.fee,
      Burn(tx.asset.id, tx.quantity)
    ).map(Diff(tx = tx, scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)) |+| _)

  def sponsor(blockchain: Blockchain, blockTime: Long)(tx: SponsorFeeTransaction): Either[ValidationError, Diff] =
    DiffsCommon.validateAsset(blockchain, tx.asset, tx.sender, issuerOnly = true).flatMap { _ =>
      Either.cond(
        !blockchain.hasAssetScript(tx.asset),
        Diff(
          tx = tx,
          portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map.empty)),
          sponsorship = Map(tx.asset           -> SponsorshipValue(tx.minSponsoredAssetFee.getOrElse(0))),
          scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)
        ),
        GenericError("Sponsorship smart assets is disabled.")
      )
    }
}
