package com.wavesplatform.state.reader

import cats.data.Ior
import cats.implicits._
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.{BlockId, BlockInfo}
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state._
import com.wavesplatform.state.extensions.composite.{CompositeAddressTransactions, CompositeDistributions}
import com.wavesplatform.state.extensions.{AddressTransactions, Distributions}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.{AliasDoesNotExist, AliasIsDisabled, GenericError}
import com.wavesplatform.transaction.assets.UpdateAssetInfoTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, Transaction}

final case class CompositeBlockchain(
    inner: Blockchain,
    maybeDiff: Option[Diff] = None,
    newBlock: Option[Block] = None,
    carry: Long = 0,
    reward: Option[Long] = None
) extends Blockchain {
  override val settings: BlockchainSettings = inner.settings

  def diff: Diff = maybeDiff.getOrElse(Diff.empty)

  override def balance(address: Address, assetId: Asset): Long =
    inner.balance(address, assetId) + diff.portfolios.getOrElse(address, Portfolio.empty).balanceOf(assetId)

  override def leaseBalance(address: Address): LeaseBalance = {
    cats.Monoid.combine(inner.leaseBalance(address), diff.portfolios.getOrElse(address, Portfolio.empty).lease)
  }

  override def assetScriptWithComplexity(asset: IssuedAsset): Option[(Script, Long)] =
    maybeDiff
      .flatMap(_.assetScripts.get(asset))
      .getOrElse(inner.assetScriptWithComplexity(asset))

  override def hasAssetScript(asset: IssuedAsset): Boolean = maybeDiff.flatMap(_.assetScripts.get(asset)) match {
    case Some(s) => s.nonEmpty
    case None    => inner.hasAssetScript(asset)
  }

  override def assetDescription(asset: IssuedAsset): Option[AssetDescription] = {
    val script: Option[Script] = assetScript(asset)

    val fromDiff = diff.issuedAssets
      .get(asset)
      .map {
        case (static, info, volume) =>
          val sponsorship = diff.sponsorship.get(asset).fold(0L) {
            case SponsorshipValue(sp) => sp
            case SponsorshipNoInfo    => 0L
          }

          AssetDescription(
            static.source,
            static.issuer,
            info.name,
            info.description,
            static.decimals,
            volume.isReissuable,
            volume.volume,
            Height @@ this.height,
            script,
            sponsorship
          )
      }

    val assetDescription =
      inner
        .assetDescription(asset)
        .orElse(fromDiff)
        .map { description =>
          diff.updatedAssets
            .get(asset)
            .fold(description) {
              case Ior.Left(info) =>
                description.copy(name = info.name, description = info.description, lastUpdatedAt = info.lastUpdatedAt)
              case Ior.Right(vol) =>
                description.copy(reissuable = description.reissuable && vol.isReissuable, totalVolume = description.totalVolume + vol.volume)
              case Ior.Both(info, vol) =>
                description
                  .copy(
                    reissuable = description.reissuable && vol.isReissuable,
                    totalVolume = description.totalVolume + vol.volume,
                    name = info.name,
                    description = info.description,
                    lastUpdatedAt = info.lastUpdatedAt
                  )
            }
        }
        .map { description =>
          diff.sponsorship
            .get(asset)
            .fold(description) {
              case SponsorshipNoInfo   => description.copy(sponsorship = 0L)
              case SponsorshipValue(v) => description.copy(sponsorship = v)
            }
        }

    assetDescription map { z =>
      diff.transactions
        .foldLeft(z.copy(script = script)) {
          case (acc, (_, (ut: UpdateAssetInfoTransaction, _))) => acc.copy(name = ut.name, description = ut.description)
          case (acc, _)                                        => acc
        }
    }
  }

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = {
    inner.leaseDetails(leaseId).map(ld => ld.copy(isActive = diff.leaseState.getOrElse(leaseId, ld.isActive))) orElse
      diff.transactions.get(leaseId).collect {
        case (lt: LeaseTransaction, _) =>
          LeaseDetails(lt.sender, lt.recipient, this.height, lt.amount, diff.leaseState(lt.id()))
      }
  }

  override def transferById(id: BlockId): Option[(Int, TransferTransaction)] = {
    diff.transactions
      .get(id)
      .collect {
        case (tx: TransferTransaction, _) => (height, tx)
      }
      .orElse(inner.transferById(id))
  }

  override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] =
    diff.transactions
      .get(id)
      .map(t => (this.height, t._1))
      .orElse(inner.transactionInfo(id))

  override def transactionHeight(id: ByteStr): Option[Int] =
    diff.transactions
      .get(id)
      .map(_ => this.height)
      .orElse(inner.transactionHeight(id))

  override def height: Int = inner.height + newBlock.fold(0)(_ => 1)

  override def resolveAlias(alias: Alias): Either[ValidationError, Address] = inner.resolveAlias(alias) match {
    case l @ Left(AliasIsDisabled(_)) => l
    case Right(addr)                  => Right(diff.aliases.getOrElse(alias, addr))
    case Left(_)                      => diff.aliases.get(alias).toRight(AliasDoesNotExist(alias))
  }

  override def collectActiveLeases(from: Int, to: Int)(filter: LeaseTransaction => Boolean): Seq[LeaseTransaction] =
    CompositeBlockchain.collectActiveLeases(inner, maybeDiff, height, from, to)(filter)

  override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A] = {
    val b = Map.newBuilder[Address, A]
    for ((a, p) <- diff.portfolios if p.lease != LeaseBalance.empty || p.balance != 0) {
      pf.runWith(b += a -> _)(a -> this.wavesPortfolio(a))
    }

    inner.collectLposPortfolios(pf) ++ b.result()
  }

  override def invokeScriptResult(txId: TransactionId): Either[ValidationError, InvokeScriptResult] = {
    diff.scriptResults
      .get(txId)
      .toRight(GenericError("InvokeScript result not found"))
      .orElse(inner.invokeScriptResult(txId))
  }

  override def containsTransaction(tx: Transaction): Boolean = diff.transactions.contains(tx.id()) || inner.containsTransaction(tx)

  override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee =
    diff.orderFills.get(orderId).orEmpty.combine(inner.filledVolumeAndFee(orderId))

  override def balanceSnapshots(address: Address, from: Int, to: BlockId): Seq[BalanceSnapshot] = {
    if (inner.heightOf(to).isDefined || maybeDiff.isEmpty) {
      inner.balanceSnapshots(address, from, to)
    } else {
      val balance = this.balance(address)
      val lease   = this.leaseBalance(address)
      val bs      = BalanceSnapshot(height, Portfolio(balance, lease, Map.empty))
      if (inner.height > 0 && from < this.height) bs +: inner.balanceSnapshots(address, from, to) else Seq(bs)
    }
  }

  override def accountScriptWithComplexity(address: Address): Option[(Script, Long, Map[String, Long])] = {
    diff.scripts.get(address) match {
      case None            => inner.accountScriptWithComplexity(address)
      case Some(None)      => None
      case Some(Some(scr)) => Some(scr)
    }
  }

  override def hasScript(address: Address): Boolean = {
    diff.scripts.get(address) match {
      case None          => inner.hasScript(address)
      case Some(None)    => false
      case Some(Some(_)) => true
    }
  }

  override def accountDataKeys(acc: Address): Set[String] = {
    val fromInner = inner.accountDataKeys(acc)
    val fromDiff  = diff.accountData.get(acc).toSeq.flatMap(_.data.keys)
    fromInner ++ fromDiff
  }

  override def accountData(acc: Address): AccountDataInfo = {
    val fromInner = inner.accountData(acc)
    val fromDiff  = diff.accountData.get(acc).orEmpty
    fromInner.combine(fromDiff)
  }

  override def accountData(acc: Address, key: String): Option[DataEntry[_]] = {
    val diffData = diff.accountData.get(acc).orEmpty
    diffData.data.get(key).orElse(inner.accountData(acc, key))
  }

  override def lastBlock: Option[Block] = newBlock.orElse(inner.lastBlock)

  override def carryFee: Long = carry

  override def score: BigInt = newBlock.fold(BigInt(0))(_.blockScore()) + inner.score

  private def filterById(blockId: BlockId): Option[Block] = newBlock.filter(_.uniqueId == blockId)
  private def filterByHeight(height: Int): Option[Block]  = newBlock.filter(_ => this.height == height)

  private def blockInfo(block: Block): BlockInfo =
    BlockInfo(block.header, block.bytes().length, block.transactionData.size, block.signature)

  override def blockInfo(height: Int): Option[BlockInfo] =
    filterByHeight(height).map(blockInfo) orElse inner.blockInfo(height)
  override def blockInfo(blockId: ByteStr): Option[BlockInfo] =
    filterById(blockId).map(blockInfo) orElse inner.blockInfo(blockId)

  override def blockBytes(height: Int): Option[Array[Byte]]      = filterByHeight(height).map(_.bytes()) orElse inner.blockBytes(height)
  override def blockBytes(blockId: ByteStr): Option[Array[Byte]] = filterById(blockId).map(_.bytes()) orElse inner.blockBytes(blockId)

  override def heightOf(blockId: ByteStr): Option[Int] = filterById(blockId).map(_ => height) orElse inner.heightOf(blockId)

  /** Returns the most recent block IDs, starting from the most recent  one */
  override def lastBlockIds(howMany: Int): Seq[ByteStr] =
    if (howMany <= 0) Seq.empty else newBlock.map(_.uniqueId).toSeq ++ inner.lastBlockIds(howMany - 1)

  /** Returns a chain of blocks starting with the block with the given ID (from oldest to newest) */
  override def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]] =
    for {
      ids <- inner.blockIdsAfter(parentSignature, howMany)
      newId = newBlock.filter(_.header.reference == parentSignature).map(_.uniqueId).fold(Seq.empty[ByteStr])(Seq(_))
    } yield newId ++ ids

  override def parentHeader(block: BlockHeader, back: Int): Option[BlockHeader] = inner.parentHeader(block, back)

  override def totalFee(height: Int): Option[Long] = inner.totalFee(height)

  /** Features related */
  override def approvedFeatures: Map[Short, Int] = inner.approvedFeatures

  override def activatedFeatures: Map[Short, Int] = inner.activatedFeatures

  override def featureVotes(height: Int): Map[Short, Int] = inner.featureVotes(height)

  /** Block reward related */
  override def blockReward(height: Int): Option[Long] = reward.filter(_ => this.height == height) orElse inner.blockReward(height)

  override def lastBlockReward: Option[Long] = reward.orElse(inner.lastBlockReward)

  override def blockRewardVotes(height: Int): Seq[Long] = inner.blockRewardVotes(height)

  override def wavesAmount(height: Int): BigInt = inner.wavesAmount(height)

  override def hitSourceAtHeight(height: Int): Option[ByteStr] = inner.hitSourceAtHeight(height)
}

object CompositeBlockchain extends AddressTransactions.Prov[CompositeBlockchain] with Distributions.Prov[CompositeBlockchain] {
  def addressTransactions(bu: CompositeBlockchain): AddressTransactions =
    new CompositeAddressTransactions(bu.inner, Height @@ bu.height, () => bu.maybeDiff)

  def distributions(bu: CompositeBlockchain): Distributions =
    new CompositeDistributions(bu, bu.inner, () => bu.maybeDiff)

  def collectActiveLeases(inner: Blockchain, maybeDiff: Option[Diff], height: Int, from: Int, to: Int)(
      filter: LeaseTransaction => Boolean
  ): Seq[LeaseTransaction] = {
    val innerActiveLeases = inner.collectActiveLeases(from, to)(filter)
    maybeDiff match {
      case Some(ng) if to == height =>
        val cancelledInLiquidBlock = ng.leaseState.collect {
          case (id, false) => id
        }.toSet
        val addedInLiquidBlock = ng.transactions.collect {
          case (id, (lt: LeaseTransaction, _)) if !cancelledInLiquidBlock(id) => lt
        }
        innerActiveLeases.filterNot(lt => cancelledInLiquidBlock(lt.id())) ++ addedInLiquidBlock
      case _ => innerActiveLeases
    }
  }
}
