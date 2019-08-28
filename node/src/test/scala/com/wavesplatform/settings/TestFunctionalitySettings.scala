package com.wavesplatform.settings

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.utils.Implicits._

import scala.concurrent.duration._

object TestFunctionalitySettings {
  val Enabled = FunctionalitySettings(
    featureCheckBlocksPeriod = 10000,
    blocksForFeatureActivation = 9000,
    allowTemporaryNegativeUntil = 0L,
    generationBalanceDepthFrom50To1000AfterHeight = 0,
    minimalGeneratingBalanceAfter = 0L,
    allowTransactionsFromFutureUntil = Long.MaxValue,
    allowUnissuedAssetsUntil = 0L,
    allowInvalidReissueInSameBlockUntilTimestamp = 0L,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 0L,
    resetEffectiveBalancesAtHeight = 0,
    blockVersion3AfterHeight = 0,
    preActivatedFeatures = Seq(
      BlockchainFeatures.SmartAccounts,
      BlockchainFeatures.SmartAssets,
      BlockchainFeatures.FairPoS,
      BlockchainFeatures.Ride4DApps,
      // BlockchainFeatures.BlockReward
    ).map(_ at 0).toMap,
    doubleFeaturesPeriodsAfterHeight = Int.MaxValue,
    maxTransactionTimeBackOffset = 120.minutes,
    maxTransactionTimeForwardOffset = 90.minutes,
    blockRewardSettings = BlockRewardSettings(0, 0, 0, 1, 1, 1, 1)
  )

  val Stub: FunctionalitySettings = Enabled.copy(featureCheckBlocksPeriod = 100, blocksForFeatureActivation = 90)

  val EmptyFeaturesSettings: FeaturesSettings =
    FeaturesSettings(autoShutdownOnUnsupportedFeature = false, List.empty)
}
