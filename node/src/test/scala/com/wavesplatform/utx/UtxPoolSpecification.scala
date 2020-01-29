package com.wavesplatform.utx

import java.nio.file.Files

import cats.data.NonEmptyList
import com.wavesplatform
import com.wavesplatform._
import com.wavesplatform.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.TransactionsOrdering
import com.wavesplatform.database.openDB
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.{StorageFactory, randomSig}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.compiler.{CompilerContext, ExpressionCompiler}
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.mining._
import com.wavesplatform.settings._
import com.wavesplatform.state._
import com.wavesplatform.state.diffs._
import com.wavesplatform.state.extensions.Distributions
import com.wavesplatform.state.utils.TestLevelDB
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxValidationError.SenderIsBlacklisted
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{Asset, Transaction, _}
import com.wavesplatform.utils.Implicits.SubjectOps
import com.wavesplatform.utils.Time
import monix.reactive.subjects.Subject
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers, PrivateMethodTester}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.concurrent.duration._

private object UtxPoolSpecification {
  private val ignoreSpendableBalanceChanged = Subject.empty[(Address, Asset)]

  final case class TempDB(fs: FunctionalitySettings, dbSettings: DBSettings) {
    val path   = Files.createTempDirectory("leveldb-test")
    val db     = openDB(path.toAbsolutePath.toString)
    val writer = TestLevelDB.withFunctionalitySettings(db, ignoreSpendableBalanceChanged, fs, dbSettings)

    sys.addShutdownHook {
      db.close()
      TestHelpers.deleteRecursively(path)
    }
  }
}

class UtxPoolSpecification
    extends FreeSpec
    with Matchers
    with MockFactory
    with PropertyChecks
    with TransactionGen
    with NoShrink
    with BlocksTransactionsHelpers
    with WithDomain
    with PrivateMethodTester {
  val PoolDefaultMaxBytes = 50 * 1024 * 1024 // 50 MB

  import FeeValidation.{ScriptExtraFee => extraFee}
  import FunctionalitySettings.TESTNET.{maxTransactionTimeBackOffset => maxAge}
  import UtxPoolSpecification._

  private def mkBlockchain(senderAccount: Address, senderBalance: Long) = {
    val genesisSettings = TestHelpers.genesisSettings(Map(senderAccount -> senderBalance))
    val origSettings    = WavesSettings.default()
    val settings = origSettings.copy(
      blockchainSettings = BlockchainSettings(
        'T',
        FunctionalitySettings.TESTNET.copy(
          preActivatedFeatures = Map(
            BlockchainFeatures.MassTransfer.id  -> 0,
            BlockchainFeatures.SmartAccounts.id -> 0,
            BlockchainFeatures.Ride4DApps.id    -> 0
          )
        ),
        genesisSettings,
        RewardsSettings.TESTNET
      ),
      featuresSettings = origSettings.featuresSettings.copy(autoShutdownOnUnsupportedFeature = false)
    )

    val dbContext = TempDB(settings.blockchainSettings.functionalitySettings, settings.dbSettings)
    val bcu       = StorageFactory(settings, dbContext.db, new TestTime(), ignoreSpendableBalanceChanged)
    bcu.processBlock(Block.genesis(genesisSettings).explicitGet()).explicitGet()
    bcu
  }

  private def transfer(sender: KeyPair, maxAmount: Long, time: Time) =
    (for {
      amount    <- chooseNum(1, (maxAmount * 0.9).toLong)
      recipient <- accountGen
      fee       <- chooseNum(extraFee, (maxAmount * 0.1).toLong)
    } yield TransferTransactionV1.selfSigned(Waves, sender, recipient, amount, time.getTimestamp(), Waves, fee, Array.empty[Byte]).explicitGet())
      .label("transferTransaction")

  private def transferV2(sender: KeyPair, maxAmount: Long, time: Time) =
    (for {
      amount    <- chooseNum(1, (maxAmount * 0.9).toLong)
      recipient <- accountGen
      fee       <- chooseNum(extraFee, (maxAmount * 0.1).toLong)
    } yield TransferTransactionV2.selfSigned(Waves, sender, recipient, amount, time.getTimestamp(), Waves, fee, Array.empty[Byte]).explicitGet())
      .label("transferTransactionV2")

  private def transferWithRecipient(sender: KeyPair, recipient: PublicKey, maxAmount: Long, time: Time) =
    (for {
      amount <- chooseNum(1, (maxAmount * 0.9).toLong)
      fee    <- chooseNum(extraFee, (maxAmount * 0.1).toLong)
    } yield TransferTransactionV1.selfSigned(Waves, sender, recipient, amount, time.getTimestamp(), Waves, fee, Array.empty[Byte]).explicitGet())
      .label("transferWithRecipient")

  private def massTransferWithRecipients(sender: KeyPair, recipients: List[PublicKey], maxAmount: Long, time: Time) = {
    val amount    = maxAmount / (recipients.size + 1)
    val transfers = recipients.map(r => ParsedTransfer(r.toAddress, amount))
    val minFee    = FeeValidation.FeeConstants(TransferTransaction.typeId) + FeeValidation.FeeConstants(MassTransferTransaction.typeId) * transfers.size
    val txs = for { fee <- chooseNum(minFee, amount) } yield MassTransferTransaction
      .selfSigned(Waves, sender, transfers, time.getTimestamp(), fee, Array.empty[Byte])
      .explicitGet()
    txs.label("transferWithRecipient")
  }

  private val stateGen = for {
    sender        <- accountGen.label("sender")
    senderBalance <- positiveLongGen.label("senderBalance")
    if senderBalance > 100000L
  } yield {
    val bcu = mkBlockchain(sender, senderBalance)
    (sender, senderBalance, bcu)
  }

  private val stateWithTransfer = for {
    (sender, balance, bcu) <- stateGen
    time = new TestTime
    transfer <- transfer(sender, balance, time)
  } yield (time, bcu, transfer)

  private val twoOutOfManyValidPayments = (for {
    (sender, senderBalance, bcu) <- stateGen
    recipient                    <- accountGen
    n                            <- chooseNum(3, 10)
    fee                          <- chooseNum(extraFee, (senderBalance * 0.01).toLong)
    offset                       <- chooseNum(1000L, 2000L)
  } yield {
    val time = new TestTime()
    val utx =
      new UtxPoolImpl(
        time,
        bcu,
        ignoreSpendableBalanceChanged,
        UtxSettings(10, PoolDefaultMaxBytes, 1000, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = false)
      )
    val amountPart = (senderBalance - fee) / 2 - fee
    val txs        = for (_ <- 1 to n) yield createWavesTransfer(sender, recipient, amountPart, fee, time.getTimestamp()).explicitGet()
    (utx, time, txs, (offset + 1000).millis)
  }).label("twoOutOfManyValidPayments")

  private val emptyUtxPool = stateGen
    .map {
      case (sender, _, bcu) =>
        val time = new TestTime()
        val utxPool =
          new UtxPoolImpl(
            time,
            bcu,
            ignoreSpendableBalanceChanged,
            UtxSettings(10, PoolDefaultMaxBytes, 1000, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = false)
          )
        (sender, bcu, utxPool)
    }
    .label("emptyUtxPool")

  private val withValidPayments = (for {
    (sender, senderBalance, bcu) <- stateGen
    recipient                    <- accountGen
    time = new TestTime()
    txs <- Gen.nonEmptyListOf(transferWithRecipient(sender, recipient, senderBalance / 10, time))
  } yield {
    val settings =
      UtxSettings(10, PoolDefaultMaxBytes, 1000, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = false)
    val utxPool = new UtxPoolImpl(time, bcu, ignoreSpendableBalanceChanged, settings)
    txs.foreach(utxPool.putIfNew(_))
    (sender, bcu, utxPool, time, settings)
  }).label("withValidPayments")

  private val withValidPaymentsNotAdded = (for {
    (sender, senderBalance, bcu) <- stateGen
    recipient                    <- accountGen
    time = new TestTime()
    txs <- Gen.nonEmptyListOf(transferWithRecipient(sender, recipient, senderBalance / 10, time))
  } yield {
    val settings =
      UtxSettings(txs.size, PoolDefaultMaxBytes, 1000, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = false)
    val utxPool = new UtxPoolImpl(time, bcu, ignoreSpendableBalanceChanged, settings)
    (sender, bcu, utxPool, txs, time, settings)
  }).label("withValidPayments")

  private val withBlacklisted = (for {
    (sender, senderBalance, bcu) <- stateGen
    recipient                    <- accountGen
    time = new TestTime()
    txs <- Gen.nonEmptyListOf(transferWithRecipient(sender, recipient, senderBalance / 10, time)) // @TODO: Random transactions
  } yield {
    val settings =
      UtxSettings(
        10,
        PoolDefaultMaxBytes,
        1000,
        Set(sender.stringRepr),
        Set.empty,
        allowTransactionsFromSmartAccounts = true,
        allowSkipChecks = false
      )
    val utxPool = new UtxPoolImpl(time, bcu, ignoreSpendableBalanceChanged, settings)
    (sender, utxPool, txs)
  }).label("withBlacklisted")

  private val withBlacklistedAndAllowedByRule = (for {
    (sender, senderBalance, bcu) <- stateGen
    recipient                    <- accountGen
    time = new TestTime()
    txs <- Gen.nonEmptyListOf(transferWithRecipient(sender, recipient, senderBalance / 10, time)) // @TODO: Random transactions
  } yield {
    val settings =
      UtxSettings(
        txs.length,
        PoolDefaultMaxBytes,
        1000,
        Set(sender.stringRepr),
        Set(recipient.stringRepr),
        allowTransactionsFromSmartAccounts = true,
        allowSkipChecks = false
      )
    val utxPool = new UtxPoolImpl(time, bcu, ignoreSpendableBalanceChanged, settings)
    (sender, utxPool, txs)
  }).label("withBlacklistedAndAllowedByRule")

  private def massTransferWithBlacklisted(allowRecipients: Boolean) =
    (for {
      (sender, senderBalance, bcu) <- stateGen
      addressGen = Gen.listOf(accountGen).filter(list => if (allowRecipients) list.nonEmpty else true)
      recipients <- addressGen.map(_.map(_.publicKey))
      time = new TestTime()
      txs <- Gen.nonEmptyListOf(massTransferWithRecipients(sender, recipients, senderBalance / 10, time))
    } yield {
      val whitelist: Set[String] = if (allowRecipients) recipients.map(_.stringRepr).toSet else Set.empty
      val settings =
        UtxSettings(
          txs.length,
          PoolDefaultMaxBytes,
          1000,
          Set(sender.stringRepr),
          whitelist,
          allowTransactionsFromSmartAccounts = true,
          allowSkipChecks = false
        )
      val utxPool = new UtxPoolImpl(time, bcu, ignoreSpendableBalanceChanged, settings)
      (sender, utxPool, txs)
    }).label("massTransferWithBlacklisted")

  private def utxTest(utxSettings: UtxSettings, txCount: Int = 10)(f: (Seq[TransferTransactionV1], UtxPool, TestTime) => Unit): Unit =
    forAll(stateGen, chooseNum(2, txCount).label("txCount")) {
      case ((sender, senderBalance, bcu), count) =>
        val time = new TestTime()

        forAll(listOfN(count, transfer(sender, senderBalance / 2, time))) { txs =>
          val utx = new UtxPoolImpl(time, bcu, ignoreSpendableBalanceChanged, utxSettings)
          f(txs, utx, time)
        }
    }

  private val dualTxGen: Gen[(UtxPool, TestTime, Seq[Transaction], Seq[Transaction])] =
    for {
      (sender, senderBalance, bcu) <- stateGen
      ts = System.currentTimeMillis()
      count1 <- chooseNum(5, 10)
      tx1    <- listOfN(count1, transfer(sender, senderBalance / 2, new TestTime(ts)))
      tx2    <- listOfN(count1, transfer(sender, senderBalance / 2, new TestTime(ts + maxAge.toMillis + 1000)))
    } yield {
      val time = new TestTime()
      val utx = new UtxPoolImpl(
        time,
        bcu,
        ignoreSpendableBalanceChanged,
        UtxSettings(10, PoolDefaultMaxBytes, 1000, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = false)
      )
      (utx, time, tx1, tx2)
    }

  private val expr: EXPR = {
    val code =
      """let x = 1
        |let y = 2
        |true""".stripMargin
    ExpressionCompiler.compile(code, CompilerContext.empty).explicitGet()
  }

  private val script: Script = ExprScript(expr).explicitGet()

  private def preconditionsGen(lastBlockId: ByteStr, master: KeyPair): Gen[Seq[Block]] =
    for {
      ts <- timestampGen
    } yield {
      val setScript = SetScriptTransaction.selfSigned(master, Some(script), 100000, ts + 1).explicitGet()
      Seq(TestBlock.create(ts + 1, lastBlockId, Seq(setScript)))
    }

  private def withScriptedAccount(scEnabled: Boolean): Gen[(KeyPair, Long, UtxPoolImpl, Long)] =
    for {
      (sender, senderBalance, bcu) <- stateGen
      preconditions                <- preconditionsGen(bcu.lastBlockId.get, sender)
    } yield {
      // val smartAccountsFs = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0))
      preconditions.foreach(b => bcu.processBlock(b).explicitGet())
      val utx = new UtxPoolImpl(
        new TestTime(),
        bcu,
        ignoreSpendableBalanceChanged,
        UtxSettings(10, PoolDefaultMaxBytes, 1000, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = scEnabled, allowSkipChecks = false)
      )

      (sender, senderBalance, utx, bcu.lastBlock.fold(0L)(_.timestamp))
    }

  private def transactionV1Gen(sender: KeyPair, ts: Long, feeAmount: Long): Gen[TransferTransactionV1] = accountGen.map { recipient =>
    TransferTransactionV1.selfSigned(Waves, sender, recipient, waves(1), ts, Waves, feeAmount, Array.emptyByteArray).explicitGet()
  }

  private def transactionV2Gen(sender: KeyPair, ts: Long, feeAmount: Long): Gen[TransferTransactionV2] = accountGen.map { recipient =>
    TransferTransactionV2.selfSigned(Waves, sender, recipient, waves(1), ts, Waves, feeAmount, Array.emptyByteArray).explicitGet()
  }

  "UTX Pool" - {
    "does not add new transactions when full" in utxTest(
      UtxSettings(1, PoolDefaultMaxBytes, 1000, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = false)
    ) { (txs, utx, _) =>
      utx.putIfNew(txs.head).resultE shouldBe 'right
      all(txs.tail.map(t => utx.putIfNew(t).resultE)) should produce("pool size limit")
    }

    "does not add new transactions when full in bytes" in utxTest(
      UtxSettings(999999, 152, 1000, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = false)
    ) { (txs, utx, _) =>
      utx.putIfNew(txs.head).resultE shouldBe 'right
      all(txs.tail.map(t => utx.putIfNew(t).resultE)) should produce("pool bytes size limit")
    }

    "adds new transactions when skip checks is allowed" in {
      forAll(stateGen) {
        case (sender, senderBalance, bcu) =>
          val time = new TestTime()

          val gen = for {
            headTransaction <- transfer(sender, senderBalance / 2, time)
            vipTransaction  <- transfer(sender, senderBalance / 2, time).suchThat(TransactionsOrdering.InUTXPool.compare(_, headTransaction) < 0)
          } yield (headTransaction, vipTransaction)

          forAll(gen, Gen.choose(0, 1).label("allowSkipChecks")) {
            case ((headTransaction, vipTransaction), allowSkipChecks) =>
              val utxSettings =
                UtxSettings(1, 152, 1, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = allowSkipChecks == 1)
              val utx = new UtxPoolImpl(time, bcu, ignoreSpendableBalanceChanged, utxSettings)

              utx.putIfNew(headTransaction).resultE shouldBe 'right
              utx.putIfNew(vipTransaction).resultE shouldBe (if (allowSkipChecks == 1) 'right else 'left)
          }
      }
    }

    "does not broadcast the same transaction twice if not allowed" in utxTest(
      utxSettings =
        UtxSettings(20, PoolDefaultMaxBytes, 1000, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = false)
    ) { (txs, utx, _) =>
      utx.putIfNew(txs.head).resultE should matchPattern { case Right(true)  => }
      utx.putIfNew(txs.head).resultE should matchPattern { case Right(false) => }
    }

    "packUnconfirmed result is limited by constraint" in forAll(dualTxGen) {
      case (utx, _, txs, _) =>
        all(txs.map(tx => utx.putIfNew(tx).resultE)) shouldBe 'right
        utx.all.size shouldEqual txs.size

        val maxNumber             = Math.max(utx.all.size / 2, 3)
        val rest                  = limitByNumber(maxNumber)
        val (packed, restUpdated) = utx.packUnconfirmed(rest, 5.seconds)

        packed.get.lengthCompare(maxNumber) should be <= 0
        if (maxNumber <= utx.all.size) restUpdated shouldBe 'full
    }

    "evicts expired transactions when packUnconfirmed is called" in forAll(dualTxGen) {
      case (utx, time, txs, _) =>
        all(txs.map(tx => utx.putIfNew(tx).resultE)) shouldBe 'right
        utx.all.size shouldEqual txs.size

        time.advance(maxAge + 1000.millis)

        val (packed, _) = utx.packUnconfirmed(limitByNumber(100), 5.seconds)
        packed shouldBe 'empty
        utx.all shouldBe 'empty
    }

    "evicts one of mutually invalid transactions when packUnconfirmed is called" in forAll(twoOutOfManyValidPayments) {
      case (utx, time, txs, offset) =>
        all(txs.map(tx => utx.putIfNew(tx).resultE)) shouldBe 'right
        utx.all.size shouldEqual txs.size

        time.advance(offset)

        val (packed, _) = utx.packUnconfirmed(limitByNumber(100), 5.seconds)
        packed.get.size shouldBe 2
        utx.all.size shouldBe 2
    }

    "correctly process constraints in packUnconfirmed" in {
      withDomain(wavesplatform.history.TransfersV2ActivatedAt0WavesSettings) { d =>
        val generateBlock: Gen[(KeyPair, Block, Seq[Transaction], Seq[Transaction])] =
          for {
            richAccount   <- accountGen
            randomAccount <- accountGen
          } yield {
            val genesisBlock = UnsafeBlocks.unsafeBlock(
              reference = randomSig,
              txs = Seq(
                GenesisTransaction.create(richAccount, ENOUGH_AMT, ntpNow).explicitGet(),
                GenesisTransaction.create(randomAccount, ENOUGH_AMT, ntpNow).explicitGet(),
                SetScriptTransaction
                  .signed(
                    richAccount,
                    Some(Script.fromBase64String("AQkAAGcAAAACAHho/EXujJiPAJUhuPXZYac+rt2jYg==").explicitGet()),
                    QuickTX.FeeAmount * 4,
                    ntpNow,
                    richAccount
                  )
                  .explicitGet()
              ),
              signer = TestBlock.defaultSigner,
              version = 3,
              timestamp = ntpNow
            )
            val scripted   = (1 to 100).flatMap(_ => QuickTX.transferV2(richAccount, randomAccount.toAddress, timestamp = ntpTimestampGen).sample)
            val unscripted = (1 to 100).flatMap(_ => QuickTX.transfer(randomAccount, timestamp = ntpTimestampGen).sample)
            (richAccount, genesisBlock, scripted, unscripted)
          }

        val Some((_, block, scripted, unscripted)) = generateBlock.sample
        d.blockchainUpdater.processBlock(block) shouldBe 'right

        val utx = new UtxPoolImpl(
          ntpTime,
          d.blockchainUpdater,
          ignoreSpendableBalanceChanged,
          UtxSettings(9999999, PoolDefaultMaxBytes, 999999, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = false)
        )
        all((scripted ++ unscripted).map(tx => utx.putIfNew(tx).resultE)) shouldBe 'right

        val constraint = MultiDimensionalMiningConstraint(
          NonEmptyList.of(
            OneDimensionalMiningConstraint(1, TxEstimators.scriptRunNumber, "scriptRunNumber"),
            OneDimensionalMiningConstraint(Block.MaxTransactionsPerBlockVer3, TxEstimators.one, "KeyBlock")
          )
        )
        val (packed, _) = utx.packUnconfirmed(constraint, 5.seconds)
        packed.get.size shouldBe (unscripted.size + 1)
        packed.get.count(scripted.contains) shouldBe 1
      }
    }

    "pessimisticPortfolio" - {
      "is not empty if there are transactions" in forAll(withValidPayments) {
        case (sender, _, utxPool, _, _) =>
          utxPool.size should be > 0
          utxPool.pessimisticPortfolio(sender) should not be empty
      }

      "is empty if there is no transactions" in forAll(emptyUtxPool) {
        case (sender, _, utxPool) =>
          utxPool.size shouldBe 0
          utxPool.pessimisticPortfolio(sender) shouldBe empty
      }

      "is empty if utx pool was cleaned" in forAll(withValidPayments) {
        case (sender, _, utxPool, _, _) =>
          utxPool.removeAll(utxPool.all)
          utxPool.pessimisticPortfolio(sender) shouldBe empty
      }

      "is changed after transactions with these assets are removed" in forAll(withValidPayments) {
        case (sender, _, utxPool, time, _) =>
          val portfolioBefore = utxPool.pessimisticPortfolio(sender)
          val poolSizeBefore  = utxPool.size

          time.advance(maxAge * 2)
          utxPool.packUnconfirmed(limitByNumber(100), 5.seconds)

          poolSizeBefore should be > utxPool.size
          val portfolioAfter = utxPool.pessimisticPortfolio(sender)

          portfolioAfter should not be portfolioBefore
      }
    }

    "spendableBalance" - {
      "equal to state's portfolio if utx is empty" in forAll(emptyUtxPool) {
        case (sender, _, utxPool) =>
          val pessimisticAssetIds = {
            val p = utxPool.pessimisticPortfolio(sender)
            p.assetIds.filter(x => p.balanceOf(x) != 0)
          }

          pessimisticAssetIds shouldBe empty
      }

      "takes into account added txs" in forAll(for (pc <- withValidPaymentsNotAdded; verify <- Gen.choose(0, 1).map(_ == 1)) yield (pc, verify)) {
        case ((sender, _, utxPool, txs, _, _), verify) =>
          val emptyPf = utxPool.pessimisticPortfolio(sender)
          all(txs.map(utxPool.putIfNew(_, verify).resultE)) shouldBe 'right
          utxPool.pessimisticPortfolio(sender) should not be emptyPf
      }

      "takes into account unconfirmed transactions" in forAll(withValidPayments) {
        case (sender, state, utxPool, _, _) =>
          val basePortfolio = Distributions(state).portfolio(sender)
          val baseAssetIds  = basePortfolio.assetIds

          val pessimisticAssetIds = {
            val p = utxPool.pessimisticPortfolio(sender)
            p.assetIds.filter(x => p.balanceOf(x) != 0)
          }

          val unchangedAssetIds = baseAssetIds -- pessimisticAssetIds
          withClue("unchanged") {
            unchangedAssetIds.foreach { assetId =>
              basePortfolio.balanceOf(assetId) shouldBe basePortfolio.balanceOf(assetId)
            }
          }

          val changedAssetIds = pessimisticAssetIds -- baseAssetIds
          withClue("changed") {
            changedAssetIds.foreach { assetId =>
              basePortfolio.balanceOf(assetId) should not be basePortfolio.balanceOf(assetId)
            }
          }
      }
    }

    "blacklisting" - {
      "prevent a transfer transaction from specific addresses" in {
        val transferGen = Gen.oneOf(withBlacklisted, massTransferWithBlacklisted(allowRecipients = false))
        forAll(transferGen) {
          case (_, utxPool, txs) =>
            val r = txs.forall { tx =>
              utxPool.putIfNew(tx).resultE match {
                case Left(SenderIsBlacklisted(_)) => true
                case _                            => false
              }
            }

            r shouldBe true
            utxPool.all.size shouldEqual 0
        }
      }

      "allow a transfer transaction from blacklisted address to specific addresses" in {
        val transferGen = Gen.oneOf(withBlacklistedAndAllowedByRule, massTransferWithBlacklisted(allowRecipients = true))
        forAll(transferGen) {
          case (_, utxPool, txs) =>
            all(txs.map { t =>
              utxPool.putIfNew(t).resultE
            }) shouldBe 'right
            utxPool.all.size shouldEqual txs.size
        }
      }
    }

    "smart accounts" - {
      "signed txs from scripted account is not allowed" in {
        val enoughFeeTxWithScriptedAccount =
          for {
            (sender, senderBalance, utx, ts) <- withScriptedAccount(true)
            feeAmount                        <- choose(extraFee, senderBalance / 2)
            tx                               <- transactionV1Gen(sender, ts + 1, feeAmount)
          } yield (utx, tx)

        val (utx, tx) = enoughFeeTxWithScriptedAccount.sample.getOrElse(throw new IllegalStateException("NO SAMPLE"))
        utx.putIfNew(tx).resultE should produce("signature from scripted account")
      }

      "any transaction from scripted account is not allowed if smartAccounts disabled in utx pool" - {

        def enoughFeeTxWithScriptedAccount(version: Int): Gen[(UtxPoolImpl, TransferTransaction)] =
          for {
            (sender, senderBalance, utx, ts) <- withScriptedAccount(false)
            feeAmount                        <- choose(extraFee, senderBalance / 2)
            tx <- version match {
              case 1 => transactionV1Gen(sender, ts + 1, feeAmount)
              case 2 => transactionV2Gen(sender, ts + 1, feeAmount)
            }
          } yield (utx, tx)

        "v1" in {
          val (utx1, tx1) = enoughFeeTxWithScriptedAccount(1).sample.getOrElse(throw new IllegalStateException("NO SAMPLE"))
          utx1.putIfNew(tx1).resultE shouldBe 'left
        }
        "v2" in {
          val (utx2, tx2) = enoughFeeTxWithScriptedAccount(2).sample.getOrElse(throw new IllegalStateException("NO SAMPLE"))
          utx2.putIfNew(tx2).resultE should produce("denied from UTX pool")
        }
      }

      "when pack time limit is exceeded" - {
        "always packs the first transaction" in forAll(stateWithTransfer) {
          case (time, bcu, transfer) =>
            var timeSourceIsRunning = false
            def nanoTimeSource(): Long =
              if (timeSourceIsRunning) 100000L
              else {
                timeSourceIsRunning = true
                0L
              }
            val settings =
              UtxSettings(10, PoolDefaultMaxBytes, 1000, Set.empty, Set.empty, allowTransactionsFromSmartAccounts = true, allowSkipChecks = false)
            val utxPool = new UtxPoolImpl(time, bcu, ignoreSpendableBalanceChanged, settings, () => nanoTimeSource())

            utxPool.putIfNew(transfer).resultE.explicitGet()
            val (tx, _) = utxPool.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, 100.nanos)
            tx.get should contain(transfer)
        }
      }
    }

    "cleanup" - {
      "doesnt validate transactions which are removed" in {
        val gen = for {
          acc  <- accountGen
          acc1 <- accountGen
          tx1  <- transfer(acc, ENOUGH_AMT / 3, ntpTime)
          txs  <- Gen.nonEmptyListOf(transfer(acc1, 10000000L, ntpTime).suchThat(_.fee < tx1.fee))
        } yield (tx1, txs)

        forAll(gen) {
          case (tx1, rest) =>
            val blockchain = stub[Blockchain]
            (blockchain.settings _).when().returning(WavesSettings.default().blockchainSettings)
            (blockchain.height _).when().returning(1)
            (blockchain.activatedFeatures _).when().returning(Map.empty)

            val utx = new UtxPoolImpl(ntpTime, blockchain, ignoreSpendableBalanceChanged, WavesSettings.default().utxSettings)
            (blockchain.balance _).when(*, *).returning(ENOUGH_AMT).repeat((rest.length + 1) * 2)

            (blockchain.balance _)
              .when(*, *)
              .onCall { (_: Address, _: Asset) =>
                utx.removeAll(rest)
                ENOUGH_AMT
              }
              .once()
            (blockchain.balance _).when(*, *).returning(ENOUGH_AMT)

            (blockchain.leaseBalance _).when(*).returning(LeaseBalance(0, 0))
            (blockchain.accountScriptWithComplexity _).when(*).returning(None)
            (blockchain.lastBlock _).when().returning(Some(TestBlock.create(Nil)))

            utx.putIfNew(tx1).resultE shouldBe 'right
            all(rest.map(utx.putIfNew(_).resultE)) shouldBe 'right
            utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, Duration.Inf) should matchPattern {
              case (Some(Seq(`tx1`)), _) => // Success
            }
            utx.all shouldBe Seq(tx1)
        }
      }
    }

    "priority pool" - {
      "returning transactions in start of utx" in {
        val gen = for {
          acc         <- accountGen
          acc1        <- accountGen
          acc2        <- accountGen
          tx1         <- transferV2(acc, ENOUGH_AMT / 3, ntpTime)
          nonScripted <- Gen.nonEmptyListOf(transferV2(acc1, 10000000L, ntpTime).suchThat(_.fee < tx1.fee))
          scripted    <- Gen.nonEmptyListOf(transferV2(acc2, 10000000L, ntpTime).suchThat(_.fee < tx1.fee))
        } yield (tx1, nonScripted, scripted)

        val Right((testScript, testScriptComplexity)) = ScriptCompiler.compile(
          """
              |{-# STDLIB_VERSION 2 #-}
              |{-# CONTENT_TYPE EXPRESSION #-}
              |{-# SCRIPT_TYPE ACCOUNT #-}
              |true
              |""".stripMargin,
          ScriptEstimatorV1
        )

        def assertPortfolios(utx: UtxPool, transactions: Seq[TransferTransactionV2]): Unit = {
          val portfolios = transactions.groupBy(_.sender.toAddress).map {
            case (addr, transactions) =>
              val amt = transactions.map(tx => -(tx.amount + tx.fee)).sum
              (addr, amt)
          }
          portfolios.foreach {
            case (addr, balance) =>
              val pf = utx.pessimisticPortfolio(addr)
              pf.balance shouldBe balance
          }
        }

        forAll(gen) {
          case (tx1, nonScripted, scripted) =>
            val blockchain = stub[Blockchain]
            (blockchain.settings _).when().returning(WavesSettings.default().blockchainSettings)
            (blockchain.height _).when().returning(1)
            (blockchain.activatedFeatures _).when().returning(Map(BlockchainFeatures.SmartAccounts.id -> 0))

            val utx = new UtxPoolImpl(ntpTime, blockchain, ignoreSpendableBalanceChanged, WavesSettings.default().utxSettings)
            (blockchain.balance _).when(*, *).returning(ENOUGH_AMT)
            (blockchain.leaseBalance _).when(*).returning(LeaseBalance(0, 0))

            (blockchain.accountScriptWithComplexity _).when(scripted.head.sender.toAddress).returning(Some(testScript -> testScriptComplexity))
            (blockchain.hasScript _).when(scripted.head.sender.toAddress).returning(true)
            (blockchain.accountScriptWithComplexity _).when(*).returning(None)
            (blockchain.lastBlock _).when().returning(Some(TestBlock.create(Nil)))
            (blockchain.transactionHeight _).when(*).returning(None)

            utx.putIfNew(tx1).resultE shouldBe 'right
            val minedTxs = scripted.tail ++ nonScripted
            utx.addAndCleanup(minedTxs, priority = true)

            utx.packUnconfirmed(
              MultiDimensionalMiningConstraint(NonEmptyList.of(OneDimensionalMiningConstraint(1, TxEstimators.one, ""))),
              Duration.Inf
            ) match {
              case (Some(txs: Seq[_]), _) => txs shouldBe Seq(minedTxs.head)
            }
            val expectedTxs = minedTxs :+ tx1
            utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, Duration.Inf) match {
              case (Some(txs: Seq[_]), _) => txs shouldBe expectedTxs
            }
            utx.all shouldBe expectedTxs
            assertPortfolios(utx, expectedTxs)

            val (left, right) = {
              val (left, right) = minedTxs.zipWithIndex.partition(kv => kv._2 % 2 == 0)
              (left.map(_._1), right.map(_._1))
            }

            utx.removeAll(left)
            val expectedTxs1 = right :+ tx1
            assertPortfolios(utx, expectedTxs1)
            all(right.map(utx.putIfNew(_).resultE)) shouldBe Right(true)
            utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, Duration.Inf) match {
              case (Some(txs: Seq[_]), _) =>
                txs shouldBe expectedTxs1
            }
            utx.all shouldBe expectedTxs1
            assertPortfolios(utx, expectedTxs1)

            utx.addAndCleanup(right, priority = true) // Should be ignored
            utx.all shouldBe expectedTxs1
            assertPortfolios(utx, expectedTxs1)

            utx.removeAll(expectedTxs1)
            utx.all shouldBe empty
            utx.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, Duration.Inf)._1 shouldBe empty
            all(expectedTxs1.map(tx => utx.pessimisticPortfolio(tx.sender))) shouldBe empty
        }
      }
    }
  }

  private def limitByNumber(n: Int): MultiDimensionalMiningConstraint = MultiDimensionalMiningConstraint(
    OneDimensionalMiningConstraint(n, TxEstimators.one, "one"),
    OneDimensionalMiningConstraint(n, TxEstimators.one, "one")
  )

}
