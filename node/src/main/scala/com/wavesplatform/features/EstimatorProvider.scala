package com.wavesplatform.features

import com.wavesplatform.state.Blockchain
import BlockchainFeatures.ChangeMinimumFees
import FeatureProvider._
import com.wavesplatform.lang.v1.estimator.{ScriptEstimator, ScriptEstimatorV1}
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.settings.WavesSettings

object EstimatorProvider {
  implicit class EstimatorBlockchainExt(b: Blockchain) {
    val estimator: ScriptEstimator =
      if (b.isFeatureActivated(ChangeMinimumFees)) ScriptEstimatorV2
      else ScriptEstimatorV1
  }

  implicit class EstimatorWavesSettingsExt(ws: WavesSettings) {
    val estimator: ScriptEstimator =
      if (ws.featuresSettings.supported.contains(ChangeMinimumFees.id)) ScriptEstimatorV2
      else ScriptEstimatorV1
  }
}
