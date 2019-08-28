package com.wavesplatform.settings

import com.typesafe.config.ConfigFactory
import com.wavesplatform.common.state.ByteStr
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class BlockchainSettingsSpecification extends FlatSpec with Matchers {
  "BlockchainSettings" should "read custom values" in {
    val config   = loadConfig(ConfigFactory.parseString("""waves {
        |  directory = "/waves"
        |  data-directory = "/waves/data"
        |  blockchain {
        |    type = CUSTOM
        |    custom {
        |      address-scheme-character = "C"
        |      functionality {
        |        feature-check-blocks-period = 10000
        |        blocks-for-feature-activation = 9000
        |        generation-balance-depth-from-50-to-1000-after-height = 4
        |        reset-effective-balances-at-height = 15
        |        block-version-3-after-height = 18
        |        pre-activated-features {
        |          19 = 100
        |          20 = 200
        |        }
        |        double-features-periods-after-height = 21
        |        max-transaction-time-back-offset = 55s
        |        max-transaction-time-forward-offset = 12d
        |        block-reward-settings {
        |           min-reward = 0
        |           max-reward = 800000000
        |           first-reward = 600000000
        |           reward-step = 25000000
        |           first-reward-period = 250000
        |           reward-period = 150000
        |           reward-voting-period = 10000
        |        }
        |      }
        |      genesis {
        |        timestamp = 1460678400000
        |        block-timestamp = 1460678400000
        |        signature = "BASE58BLKSGNATURE"
        |        initial-balance = 100000000000000
        |        initial-base-target = 153722867
        |        average-block-delay = 60s
        |        transactions = [
        |          {recipient = "BASE58ADDRESS1", amount = 50000000000001},
        |          {recipient = "BASE58ADDRESS2", amount = 49999999999999}
        |        ]
        |      }
        |    }
        |  }
        |}""".stripMargin))
    val settings = BlockchainSettings.fromRootConfig(config)

    settings.addressSchemeCharacter should be('C')
    settings.functionalitySettings.featureCheckBlocksPeriod should be(10000)
    settings.functionalitySettings.blocksForFeatureActivation should be(9000)
    settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight should be(4)
    settings.functionalitySettings.resetEffectiveBalancesAtHeight should be(15)
    settings.functionalitySettings.blockVersion3AfterHeight should be(18)
    settings.functionalitySettings.preActivatedFeatures should be(Map(19 -> 100, 20 -> 200))
    settings.functionalitySettings.doubleFeaturesPeriodsAfterHeight should be(21)
    settings.functionalitySettings.maxTransactionTimeBackOffset should be(55.seconds)
    settings.functionalitySettings.maxTransactionTimeForwardOffset should be(12.days)
    settings.functionalitySettings.blockRewardSettings.minReward should be(0)
    settings.functionalitySettings.blockRewardSettings.maxReward should be(800000000)
    settings.functionalitySettings.blockRewardSettings.firstReward should be(600000000)
    settings.functionalitySettings.blockRewardSettings.rewardStep should be(25000000)
    settings.functionalitySettings.blockRewardSettings.firstRewardPeriod should be(250000)
    settings.functionalitySettings.blockRewardSettings.rewardPeriod should be(150000)
    settings.functionalitySettings.blockRewardSettings.rewardVotingPeriod should be(10000)
    settings.genesisSettings.blockTimestamp should be(1460678400000L)
    settings.genesisSettings.timestamp should be(1460678400000L)
    settings.genesisSettings.signature should be(ByteStr.decodeBase58("BASE58BLKSGNATURE").toOption)
    settings.genesisSettings.initialBalance should be(100000000000000L)
    settings.genesisSettings.initialBaseTarget should be(153722867)
    settings.genesisSettings.averageBlockDelay should be(60.seconds)
    settings.genesisSettings.transactions should be(
      Seq(GenesisTransactionSettings("BASE58ADDRESS1", 50000000000001L), GenesisTransactionSettings("BASE58ADDRESS2", 49999999999999L))
    )
  }

  it should "read testnet settings" in {
    val config   = loadConfig(ConfigFactory.parseString("""waves {
        |  directory = "/waves"
        |  data-directory = "/waves/data"
        |  blockchain {
        |    type = TESTNET
        |  }
        |}""".stripMargin))
    val settings = BlockchainSettings.fromRootConfig(config)

    settings.addressSchemeCharacter should be('T')
    settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight should be(0)
    settings.functionalitySettings.resetEffectiveBalancesAtHeight should be(51500)
    settings.functionalitySettings.blockVersion3AfterHeight should be(161700)
    settings.functionalitySettings.maxTransactionTimeBackOffset should be(120.minutes)
    settings.functionalitySettings.maxTransactionTimeForwardOffset should be(90.minutes)
    settings.functionalitySettings.blockRewardSettings.minReward should be(0)
    settings.functionalitySettings.blockRewardSettings.maxReward should be(800000000)
    settings.functionalitySettings.blockRewardSettings.firstReward should be(600000000)
    settings.functionalitySettings.blockRewardSettings.rewardStep should be(25000000)
    settings.functionalitySettings.blockRewardSettings.firstRewardPeriod should be(250000)
    settings.functionalitySettings.blockRewardSettings.rewardPeriod should be(150000)
    settings.functionalitySettings.blockRewardSettings.rewardVotingPeriod should be(10000)
    settings.genesisSettings.blockTimestamp should be(1460678400000L)
    settings.genesisSettings.timestamp should be(1478000000000L)
    settings.genesisSettings.signature should be(
      ByteStr.decodeBase58("5uqnLK3Z9eiot6FyYBfwUnbyid3abicQbAZjz38GQ1Q8XigQMxTK4C1zNkqS1SVw7FqSidbZKxWAKLVoEsp4nNqa").toOption
    )
    settings.genesisSettings.initialBalance should be(10000000000000000L)

    settings.genesisSettings.transactions should be(
      Seq(
        GenesisTransactionSettings("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8", 400000000000000L),
        GenesisTransactionSettings("3NBVqYXrapgJP9atQccdBPAgJPwHDKkh6A8", 200000000000000L),
        GenesisTransactionSettings("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh", 200000000000000L),
        GenesisTransactionSettings("3NCBMxgdghg4tUhEEffSXy11L6hUi6fcBpd", 200000000000000L),
        GenesisTransactionSettings("3N18z4B8kyyQ96PhN5eyhCAbg4j49CgwZJx", 9000000000000000L)
      )
    )
  }

  it should "read mainnet settings" in {
    val config   = loadConfig(ConfigFactory.parseString("""waves {
        |  directory = "/waves"
        |  data-directory = "/waves/data"
        |  blockchain {
        |    type = MAINNET
        |  }
        |}""".stripMargin))
    val settings = BlockchainSettings.fromRootConfig(config)

    settings.addressSchemeCharacter should be('W')
    settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight should be(232000L)
    settings.functionalitySettings.resetEffectiveBalancesAtHeight should be(462000)
    settings.functionalitySettings.maxTransactionTimeBackOffset should be(120.minutes)
    settings.functionalitySettings.maxTransactionTimeForwardOffset should be(90.minutes)
    settings.functionalitySettings.blockRewardSettings.minReward should be(0)
    settings.functionalitySettings.blockRewardSettings.maxReward should be(800000000)
    settings.functionalitySettings.blockRewardSettings.firstReward should be(600000000)
    settings.functionalitySettings.blockRewardSettings.rewardStep should be(25000000)
    settings.functionalitySettings.blockRewardSettings.firstRewardPeriod should be(250000)
    settings.functionalitySettings.blockRewardSettings.rewardPeriod should be(150000)
    settings.functionalitySettings.blockRewardSettings.rewardVotingPeriod should be(10000)
    settings.genesisSettings.blockTimestamp should be(1460678400000L)
    settings.genesisSettings.timestamp should be(1465742577614L)
    settings.genesisSettings.signature should be(
      ByteStr.decodeBase58("FSH8eAAzZNqnG8xgTZtz5xuLqXySsXgAjmFEC25hXMbEufiGjqWPnGCZFt6gLiVLJny16ipxRNAkkzjjhqTjBE2").toOption
    )
    settings.genesisSettings.initialBalance should be(10000000000000000L)
    settings.genesisSettings.transactions should be(
      Seq(
        GenesisTransactionSettings("3PAWwWa6GbwcJaFzwqXQN5KQm7H96Y7SHTQ", 9999999500000000L),
        GenesisTransactionSettings("3P8JdJGYc7vaLu4UXUZc1iRLdzrkGtdCyJM", 100000000L),
        GenesisTransactionSettings("3PAGPDPqnGkyhcihyjMHe9v36Y4hkAh9yDy", 100000000L),
        GenesisTransactionSettings("3P9o3ZYwtHkaU1KxsKkFjJqJKS3dLHLC9oF", 100000000L),
        GenesisTransactionSettings("3PJaDyprvekvPXPuAtxrapacuDJopgJRaU3", 100000000L),
        GenesisTransactionSettings("3PBWXDFUc86N2EQxKJmW8eFco65xTyMZx6J", 100000000L)
      )
    )
  }
}
