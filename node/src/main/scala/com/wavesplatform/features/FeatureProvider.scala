package com.wavesplatform.features

import com.wavesplatform.block.Block.{NgBlockVersion, PlainBlockVersion, RewardBlockVersion}
import com.wavesplatform.state.Blockchain

object FeatureProvider {
  implicit class FeatureProviderExt(provider: Blockchain) {
    def isFeatureActivated(feature: BlockchainFeature, height: Int = provider.height): Boolean =
      provider.activatedFeatures.get(feature.id).exists(_ <= height)

    def activatedFeaturesAt(height: Int): Set[Short] =
      provider.activatedFeatures.collect {
        case (featureId, activationHeight) if height >= activationHeight => featureId
      }.toSet

    def featureStatus(feature: Short, height: Int): BlockchainFeatureStatus =
      if (provider.activatedFeatures.get(feature).exists(_ <= height)) BlockchainFeatureStatus.Activated
      else if (provider.approvedFeatures.get(feature).exists(_ <= height)) BlockchainFeatureStatus.Approved
      else BlockchainFeatureStatus.Undefined

    def currentBlockVersion: Byte =
      if (provider.height <= provider.settings.functionalitySettings.blockVersion3AfterHeight) PlainBlockVersion
      else featureActivationHeight(BlockchainFeatures.BlockReward.id).map(_ => RewardBlockVersion).getOrElse(NgBlockVersion)

    def featureActivationHeight(feature: Short): Option[Int] = provider.activatedFeatures.get(feature)
    def featureApprovalHeight(feature: Short): Option[Int]   = provider.approvedFeatures.get(feature)
  }
}
