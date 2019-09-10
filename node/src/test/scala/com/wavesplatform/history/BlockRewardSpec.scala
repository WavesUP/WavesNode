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
import com.wavesplatform.transaction.assets.SponsorFeeTransaction
import com.wavesplatform.transaction.transfer.TransferTransactionV1
import com.wavesplatform.transaction.{Asset, GenesisTransaction}
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
        1000,
        InitialReward,
        1 * Constants.UnitsInWave,
        100
      )
    )
  )

  private def mkEmptyBlock(ref: ByteStr, signer: KeyPair): Block = TestBlock.create(ntpNow, ref, Seq.empty, signer)

  private val InitialMinerBalance = 10000 * Constants.UnitsInWave

  private val genesis = for {
    sourceAddress <- accountGen
    miner1        <- accountGen
    miner2        <- accountGen
    issuer        <- accountGen
    issue         <- issueGen(issuer, ntpTime.getTimestamp())
    asset = Asset.IssuedAsset(issue.id())
    genesisBlock = TestBlock.create(
      ntpTime.getTimestamp(),
      Seq(
        GenesisTransaction.create(sourceAddress, (Constants.TotalWaves - 30000) * Constants.UnitsInWave, ntpTime.getTimestamp()).explicitGet(),
        GenesisTransaction.create(miner1, InitialMinerBalance, ntpTime.getTimestamp()).explicitGet(),
        GenesisTransaction.create(miner2, InitialMinerBalance, ntpTime.getTimestamp()).explicitGet(),
        GenesisTransaction.create(issuer, InitialMinerBalance, ntpTime.getTimestamp()).explicitGet(),
        issue,
        SponsorFeeTransaction.selfSigned(issuer, asset, Some(12000), 100000000, ntpTime.getTimestamp()).explicitGet(),
        TransferTransactionV1
          .selfSigned(asset, issuer, sourceAddress, issue.quantity, ntpTime.getTimestamp(), Asset.Waves, 100000, Array.emptyByteArray)
          .explicitGet()
      )
    )

  } yield (sourceAddress, miner1, miner2, genesisBlock, issue)

  private val activationScenario = for {
    (sourceAddress, miner1, _, genesisBlock, _) <- genesis
    recipient                                   <- accountGen
    transfers                                   <- Gen.listOfN(10, transferGeneratorP(ntpNow, sourceAddress, recipient, 1000 * Constants.UnitsInWave))
    b2 = TestBlock.create(ntpNow, genesisBlock.uniqueId, transfers, miner1)
    b3 = mkEmptyBlock(b2.uniqueId, miner1)
    b4 = mkEmptyBlock(b3.uniqueId, miner1)
    b5 = mkEmptyBlock(b4.uniqueId, miner1)
    b6 = mkEmptyBlock(b5.uniqueId, miner1)
    b7 = mkEmptyBlock(b6.uniqueId, miner1)
    b8 = mkEmptyBlock(b7.uniqueId, miner1)
    b9 = mkEmptyBlock(b8.uniqueId, miner1)
  } yield (miner1, transfers, Seq(genesisBlock, b2), Seq(b3, b4), b5, Seq(b6, b7, b8, b9))

  "Miner receives reward as soon as the feature is activated" ignore forAll(activationScenario) {
    case (miner, transfers, b1, b2, activationBlock, b3) =>
      withDomain(rewardSettings) { d =>
        val totalFee = transfers.map(_.fee).sum

        b1.foreach(b => d.blockchainUpdater.processBlock(b).explicitGet())

        b2.foreach(b => d.blockchainUpdater.processBlock(b).explicitGet())
        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight - 1
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe false
        d.blockchainUpdater.balance(miner.toAddress) shouldBe InitialMinerBalance + totalFee

        d.blockchainUpdater.processBlock(activationBlock).explicitGet()
        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe true
//        d.blockchainUpdater.balance(miner.toAddress) shouldBe InitialReward + InitialMinerBalance

        b3.foreach(b => d.blockchainUpdater.processBlock(b).explicitGet())
        d.blockchainUpdater.balance(miner) shouldBe 4 * InitialReward + InitialMinerBalance + totalFee
      }
  }

  private def transferGen(sourceAddress: KeyPair, feeAsset: Asset): Gen[TransferTransactionV1] =
    for {
      recipient <- accountGen
      amount    <- Gen.choose(1 * Constants.UnitsInWave, 10 * Constants.UnitsInWave)
      feeAmount <- Gen.choose(12000, 20000)
    } yield TransferTransactionV1
      .selfSigned(Asset.Waves, sourceAddress, recipient, amount, ntpTime.getTimestamp(), feeAsset, feeAmount, Array.emptyByteArray)
      .explicitGet()

  private def transferSeqGen(n: Int, sourceAddress: KeyPair, feeAsset: Asset): Gen[List[TransferTransactionV1]] =
    Gen.listOfN(n, transferGen(sourceAddress, feeAsset))

  private val ngScenario = for {
    (sourceAddress, miner1, miner2, genesisBlock, issue) <- genesis
    feeAsset = Asset.IssuedAsset(issue.id())
    transfers1 <- Gen.listOfN(20, Gen.choose(10, 20).flatMap(n => transferSeqGen(n, sourceAddress, feeAsset)))
    (b2, m2s) = chainBaseAndMicro(genesisBlock.uniqueId, Seq.empty, transfers1, miner1, 3, ntpNow)
    transfers2 <- Gen.listOfN(20, Gen.choose(10, 20).flatMap(n => transferSeqGen(n, sourceAddress, feeAsset)))
    (b3, m3s) = chainBaseAndMicro(m2s(m2s.length - 2).totalResBlockSig, Seq.empty, transfers2, miner1, 3, ntpNow)
    b4        = mkEmptyBlock(m3s(m3s.length - 2).totalResBlockSig, miner1)
  } yield (sourceAddress, miner1, miner2, issue, genesisBlock, b2, m2s, b3, m3s, b4)

  "Miner receives fees from a microblock" in forAll(ngScenario) {
    case (source, miner1, miner2, issue, genesisBlock, b2, m2s, b3, m3s, b4) =>
      withDomain(rewardSettings) { d =>

        d.blockchainUpdater.processBlock(genesisBlock).explicitGet()
        d.blockchainUpdater.processBlock(b2).explicitGet()

        val initialAssetBalance = d.assetBalance(source, issue.id())

        m2s.foreach { mb =>
          d.blockchainUpdater.processMicroBlock(mb).explicitGet()
//        d.blockchainUpdater.balance(miner1) shouldBe InitialMinerBalance + (mb.transactionData.map(_.assetFee._2).sum * 4 / 10)
        }
        d.blockchainUpdater.processBlock(b3).explicitGet()

//        val sum       = m2s.flatMap(_.transactionData).map(_.assetFee._2).sum
//        val miner1Fee = sum / 5 * 2
//        d.blockchainUpdater.balance(miner1) shouldBe InitialMinerBalance + miner1Fee
//        d.blockchainUpdater.balance(miner2) shouldBe InitialMinerBalance + (sum - miner1Fee)

        m3s.foreach { mb =>
          d.blockchainUpdater.processMicroBlock(mb).explicitGet()
        }

        d.blockchainUpdater.processBlock(b4).explicitGet()

        val finalAssetBalance = d.assetBalance(source, issue.id())

        d.assetBalance(issue.sender, issue.id()) shouldBe (initialAssetBalance - finalAssetBalance)

        val totalWaves = d.blockchainUpdater.wavesDistribution(d.blockchainUpdater.height).explicitGet().values.sum
        totalWaves + d.blockchainUpdater.carryFee shouldEqual Constants.TotalWaves * Constants.UnitsInWave

        ((d.wavesBalance(miner1) + d.wavesBalance(miner2) + d.wavesBalance(issue.sender)) - 3 * InitialMinerBalance) shouldEqual 0

//        val miner2fee = sum - miner1Fee + m3s.flatMap(_.transactionData).map(_.assetFee._2).sum
//        d.blockchainUpdater.balance(miner2) shouldBe InitialMinerBalance + miner2fee
      }
  }
}
