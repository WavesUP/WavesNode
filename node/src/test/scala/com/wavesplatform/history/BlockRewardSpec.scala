package com.wavesplatform.history

import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.{Constants, FunctionalitySettings, RewardsSettings}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class BlockRewardSpec extends FreeSpec with ScalaCheckPropertyChecks with WithDomain with Matchers with TransactionGen with NoShrink {
  private val BlockRewardActivationHeight = 5
  private val NGActivationHeight          = 0
  private val InitialReward               = 6 * Constants.UnitsInWave
  private val rewardSettings = settings.copy(
    blockchainSettings = DefaultBlockchainSettings.copy(
      functionalitySettings = FunctionalitySettings(
        featureCheckBlocksPeriod = 10,
        blocksForFeatureActivation = 1,
        doubleFeaturesPeriodsAfterHeight = Int.MaxValue,
        preActivatedFeatures = Map(
          BlockchainFeatures.BlockReward.id    -> BlockRewardActivationHeight,
          BlockchainFeatures.NG.id             -> NGActivationHeight,
          BlockchainFeatures.FeeSponsorship.id -> -10
        )
      ),
      rewardsSettings = RewardsSettings(
        10,
        InitialReward,
        1 * Constants.UnitsInWave,
        4
      )
    )
  )

  private def mkEmptyBlock(ref: ByteStr, signer: KeyPair): Block = TestBlock.create(ntpNow, ref, Seq.empty, signer)

  private def mkEmptyBlockIncReward(ref: ByteStr, signer: KeyPair): Block =
    TestBlock.create(ntpNow, ref, Seq.empty, signer, rewardVote = InitialReward + 1 * Constants.UnitsInWave, version = Block.RewardBlockVersion)

  private val InitialMinerBalance = 10000 * Constants.UnitsInWave

  private val genesis = for {
    sourceAddress <- accountGen
    miner1        <- accountGen
    miner2        <- accountGen
    genesisBlock = TestBlock.create(
      ntpTime.getTimestamp(),
      Seq(
        GenesisTransaction.create(sourceAddress, (Constants.TotalWaves - 30000) * Constants.UnitsInWave, ntpTime.getTimestamp()).explicitGet(),
        GenesisTransaction.create(miner1, InitialMinerBalance, ntpTime.getTimestamp()).explicitGet(),
        GenesisTransaction.create(miner2, InitialMinerBalance, ntpTime.getTimestamp()).explicitGet(),
      )
    )

  } yield (sourceAddress, miner1, miner2, genesisBlock)

  private val activationScenario = for {
    (sourceAddress, miner1, _, genesisBlock) <- genesis
    recipient                                   <- accountGen
    transfers                                   <- Gen.listOfN(10, transferGeneratorP(ntpNow, sourceAddress, recipient, 1000 * Constants.UnitsInWave))
    b2  = TestBlock.create(ntpNow, genesisBlock.uniqueId, transfers, miner1)
    b3  = mkEmptyBlock(b2.uniqueId, miner1)
    b4  = mkEmptyBlock(b3.uniqueId, miner1)
    b5  = mkEmptyBlock(b4.uniqueId, miner1)
    b6  = mkEmptyBlock(b5.uniqueId, miner1)
    b7  = mkEmptyBlock(b6.uniqueId, miner1)
    b8  = mkEmptyBlock(b7.uniqueId, miner1)
    b9  = mkEmptyBlock(b8.uniqueId, miner1)
    b10 = mkEmptyBlock(b9.uniqueId, miner1)
    b11 = mkEmptyBlockIncReward(b10.uniqueId, miner1)
    b12 = mkEmptyBlockIncReward(b11.uniqueId, miner1)
    b13 = mkEmptyBlockIncReward(b12.uniqueId, miner1)
    b14 = mkEmptyBlockIncReward(b13.uniqueId, miner1)
    b15 = mkEmptyBlockIncReward(b14.uniqueId, miner1)
  } yield (miner1, transfers, Seq(genesisBlock, b2), Seq(b3, b4), b5, Seq(b6, b7, b8, b9), Seq(b10, b11, b12, b13, b14), b15)

  "Miner receives reward as soon as the feature is activated" in forAll(activationScenario) {
    case (miner, transfers, b1, b2, activationBlock, b3, b4, newTermBlock) =>
      withDomain(rewardSettings) { d =>
        val totalFee = transfers.map(_.fee).sum

        b1.foreach(b => d.blockchainUpdater.processBlock(b).explicitGet())

        b2.foreach(b => d.blockchainUpdater.processBlock(b).explicitGet())
        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight - 1
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe false
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight - 1) shouldBe None
        d.blockchainUpdater.balance(miner.toAddress) shouldBe InitialMinerBalance + totalFee

        d.blockchainUpdater.processBlock(activationBlock).explicitGet()
        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe true
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight) shouldBe Some(InitialReward)
        d.blockchainUpdater.balance(miner.toAddress) shouldBe InitialReward + InitialMinerBalance + totalFee

        b3.foreach(b => d.blockchainUpdater.processBlock(b).explicitGet())
        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight + 4
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe true
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight + 4) shouldBe Some(InitialReward)
        d.blockchainUpdater.balance(miner) shouldBe 5 * InitialReward + InitialMinerBalance + totalFee

        b4.foreach(b => d.blockchainUpdater.processBlock(b).explicitGet())
        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight + 9
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe true
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight + 9) shouldBe Some(InitialReward)
        d.blockchainUpdater.balance(miner) shouldBe 10 * InitialReward + InitialMinerBalance + totalFee

        val NextReward = InitialReward + 1 * Constants.UnitsInWave

        d.blockchainUpdater.processBlock(newTermBlock).explicitGet()
        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight + 10
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe true
        d.blockchainUpdater.balance(miner) shouldBe 10 * InitialReward + InitialMinerBalance + totalFee + NextReward
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight + 10) shouldBe Some(NextReward)
      }
  }
}
