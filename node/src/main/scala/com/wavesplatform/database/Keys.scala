package com.wavesplatform.database

import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.serialization.impl.TransferTxSerializer
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.utils._

object Keys {
  import KeyHelpers._

  val version: Key[Int]               = intKey("version", 0, default = 1)
  val height: Key[Int]                = intKey("height", 1)
  def score(height: Int): Key[BigInt] = Key("score", h(2, height), Option(_).fold(BigInt(0))(BigInt(_)), _.toByteArray)

  def heightOf(blockId: ByteStr): Key[Option[Int]] = Key.opt[Int]("height-of", hash(4, blockId), Ints.fromByteArray, Ints.toByteArray)

  def wavesBalanceHistory(addressId: BigInt): Key[Seq[Int]] = historyKey("waves-balance-history", 5, addressId.toByteArray)

  val WavesBalancePrefix: Short = 6
  def wavesBalance(addressId: BigInt)(height: Int): Key[Long] =
    Key("waves-balance", hAddr(WavesBalancePrefix, height, addressId), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  val AssetBalanceHistoryPrefix = 8.toShort
  def assetBalanceHistory(addressId: BigInt, asset: IssuedAsset): Key[Seq[Int]] =
    historyKey("asset-balance-history", AssetBalanceHistoryPrefix, addressId.toByteArray ++ asset.id.arr)
  val AssetBalancePrefix: Short = 9
  def assetBalance(addressId: BigInt, asset: IssuedAsset)(height: Int): Key[Long] =
    Key(
      "asset-balance",
      hBytes(AssetBalancePrefix, asset.id.arr ++ addressId.toByteArray, height),
      Option(_).fold(0L)(Longs.fromByteArray),
      Longs.toByteArray
    )

  def assetDetailsHistory(asset: IssuedAsset): Key[Seq[Int]] = historyKey("asset-details-history", 10, asset.id.arr)
  def assetDetails(asset: IssuedAsset)(height: Int): Key[(AssetInfo, AssetVolumeInfo)] =
    Key("asset-details", hBytes(11, asset.id.arr, height), readAssetDetails, writeAssetDetails)

  def leaseBalanceHistory(addressId: BigInt): Key[Seq[Int]] = historyKey("lease-balance-history", 12, addressId.toByteArray)
  def leaseBalance(addressId: BigInt)(height: Int): Key[LeaseBalance] =
    Key("lease-balance", hAddr(13, height, addressId), readLeaseBalance, writeLeaseBalance)
  def leaseStatusHistory(leaseId: ByteStr): Key[Seq[Int]] = historyKey("lease-status-history", 14, leaseId.arr)
  val LeaseStatusPrefix: Short                            = 15
  def leaseStatus(leaseId: ByteStr)(height: Int): Key[Boolean] =
    Key("lease-status", hBytes(LeaseStatusPrefix, leaseId.arr, height), _(0) == 1, active => Array[Byte](if (active) 1 else 0))

  def filledVolumeAndFeeHistory(orderId: ByteStr): Key[Seq[Int]] = historyKey("filled-volume-and-fee-history", 16, orderId.arr)
  def filledVolumeAndFee(orderId: ByteStr)(height: Int): Key[VolumeAndFee] =
    Key("filled-volume-and-fee", hBytes(17, orderId.arr, height), readVolumeAndFee, writeVolumeAndFee)

  def changedAddresses(height: Int): Key[Seq[BigInt]] = Key("changed-addresses", h(18, height), readBigIntSeq, writeBigIntSeq)

  def addressIdOfAlias(alias: Alias): Key[Option[BigInt]] = Key.opt("address-id-of-alias", bytes(19, alias.bytes.arr), BigInt(_), _.toByteArray)

  val lastAddressId: Key[Option[BigInt]] = Key.opt("last-address-id", Array[Byte](0, 20), BigInt(_), _.toByteArray)

  def addressId(address: Address): Key[Option[BigInt]] = Key.opt("address-id", bytes(21, address.bytes.arr), BigInt(_), _.toByteArray)
  def idToAddress(id: BigInt): Key[Address]            = Key("id-to-address", bytes(22, id.toByteArray), Address.fromBytes(_).explicitGet(), _.bytes.arr)

  def addressScriptHistory(addressId: BigInt): Key[Seq[Int]] = historyKey("address-script-history", 23, addressId.toByteArray)
  def addressScript(addressId: BigInt)(height: Int): Key[Option[AccountScriptInfo]] =
    Key.opt("address-script", hAddr(24, height, addressId), readAccountScriptInfo, writeAccountScriptInfo)

  val approvedFeatures: Key[Map[Short, Int]]  = Key("approved-features", Array[Byte](0, 25), readFeatureMap, writeFeatureMap)
  val activatedFeatures: Key[Map[Short, Int]] = Key("activated-features", Array[Byte](0, 26), readFeatureMap, writeFeatureMap)

  def dataKeyChunkCount(addressId: BigInt): Key[Int] =
    Key("data-key-chunk-count", addr(27, addressId), Option(_).fold(0)(Ints.fromByteArray), Ints.toByteArray)
  def dataKeyChunk(addressId: BigInt, chunkNo: Int): Key[Seq[String]] =
    Key("data-key-chunk", addr(28, addressId) ++ Ints.toByteArray(chunkNo), readStrings, writeStrings)

  val DataHistoryPrefix: Short = 29
  def dataHistory(addressId: BigInt, key: String): Key[Seq[Int]] =
    historyKey("data-history", DataHistoryPrefix, addressId.toByteArray ++ key.utf8Bytes)
  def data(addressId: BigInt, key: String)(height: Int): Key[Option[DataEntry[_]]] =
    Key.opt("data", hBytes(30, addressId.toByteArray ++ key.utf8Bytes, height), readDataEntry(key), writeDataEntry)

  def sponsorshipHistory(asset: IssuedAsset): Key[Seq[Int]] = historyKey("sponsorship-history", 31, asset.id.arr)
  def sponsorship(asset: IssuedAsset)(height: Int): Key[SponsorshipValue] =
    Key("sponsorship", hBytes(32, asset.id.arr, height), readSponsorship, writeSponsorship)

  def carryFee(height: Int): Key[Long] = Key("carry-fee", h(33, height), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  def assetScriptHistory(asset: IssuedAsset): Key[Seq[Int]] = historyKey("asset-script-history", 34, asset.id.arr)
  val AssetScriptPrefix                                     = 35.toShort
  def assetScript(asset: IssuedAsset)(height: Int): Key[Option[(Script, Long)]] =
    Key.opt("asset-script", hBytes(AssetScriptPrefix, asset.id.arr, height), readAssetScript, writeAssetScript)
  def assetScriptPresent(asset: IssuedAsset)(height: Int): Key[Option[Unit]] =
    Key.opt("asset-script", hBytes(AssetScriptPrefix, asset.id.arr, height), _ => (), _ => Array[Byte]())

  val safeRollbackHeight: Key[Int] = intKey("safe-rollback-height", 36)

  def changedDataKeys(height: Int, addressId: BigInt): Key[Seq[String]] =
    Key("changed-data-keys", hAddr(37, height, addressId), readStrings, writeStrings)

  val BlockInfoPrefix: Short = 38

  def blockMetaAt(height: Height): Key[Option[BlockMeta]] =
    Key.opt("block-info-at-height", h(BlockInfoPrefix, height), readBlockMeta(height), writeBlockMeta)

  def blockInfoBytesAt(height: Height): Key[Option[Array[Byte]]] =
    Key.opt(
      "block-info-bytes-at-height",
      h(BlockInfoPrefix, height),
      identity,
      unsupported("Can not explicitly write block bytes")
    )

  val TransactionInfoPrefix: Short = 39
  def transactionAt(height: Height, n: TxNum): Key[Option[Transaction]] =
    Key.opt[Transaction](
      "nth-transaction-info-at-height",
      hNum(TransactionInfoPrefix, height, n),
      readTransaction,
      writeTransaction
    )

  def transferTransactionAt(height: Height, n: TxNum): Key[Option[TransferTransaction]] =
    Key(
      "nth-transaction-info-bytes-at-height",
      hNum(TransactionInfoPrefix, height, n),
      TransferTxSerializer.tryParseTransfer,
      unsupported("Can not explicitly write transfer transaction")
    )

  val AddressTransactionSeqNrPrefix: Short = 40
  def addressTransactionSeqNr(addressId: AddressId): Key[Int] =
    bytesSeqNr("address-transaction-seq-nr", AddressTransactionSeqNrPrefix, addressId.toByteArray)

  val AddressTransactionHNPrefix: Short = 41
  def addressTransactionHN(addressId: AddressId, seqNr: Int): Key[Option[(Height, Seq[(Byte, TxNum)])]] =
    Key.opt(
      "address-transaction-height-type-and-nums",
      hBytes(AddressTransactionHNPrefix, addressId.toByteArray, seqNr),
      readTransactionHNSeqAndType,
      writeTransactionHNSeqAndType
    )

  val TransactionHeightNumByIdPrefix: Short = 42
  def transactionHNById(txId: TransactionId): Key[Option[(Height, TxNum)]] =
    Key.opt(
      "transaction-height-and-nums-by-id",
      bytes(TransactionHeightNumByIdPrefix, txId.arr),
      readTransactionHN,
      writeTransactionHN
    )

  val BlockTransactionsFeePrefix: Short = 43
  def blockTransactionsFee(height: Int): Key[Long] =
    Key(
      "block-transactions-fee",
      h(BlockTransactionsFeePrefix, height),
      Longs.fromByteArray,
      Longs.toByteArray
    )

  val InvokeScriptResultPrefix: Short = 44
  def invokeScriptResult(height: Int, txNum: TxNum): Key[InvokeScriptResult] =
    Key("invoke-script-result", hNum(InvokeScriptResultPrefix, height, txNum), InvokeScriptResult.fromBytes, InvokeScriptResult.toBytes)

  val BlockRewardPrefix: Short = 45
  def blockReward(height: Int): Key[Option[Long]] =
    Key.opt("block-reward", h(BlockRewardPrefix, height), Longs.fromByteArray, Longs.toByteArray)

  val wavesAmountPrefix: Short              = 46
  def wavesAmount(height: Int): Key[BigInt] = Key("waves-amount", h(wavesAmountPrefix, height), Option(_).fold(BigInt(0))(BigInt(_)), _.toByteArray)

  val HitSourcePrefix: Short                       = 47
  def hitSource(height: Int): Key[Option[ByteStr]] = Key.opt("hit-source", h(HitSourcePrefix, height), ByteStr(_), _.arr)

  val disabledAliases: Key[Set[Alias]] = Key(
    "disabled-aliases",
    Array[Byte](0, 48),
    b => readStrings(b).map(s => Alias.create(s).explicitGet()).toSet,
    as => writeStrings(as.map(_.name).toSeq)
  )

  val AssetStaticInfoPrefix: Short = 49
  def assetStaticInfo(asset: IssuedAsset): Key[Option[AssetStaticInfo]] =
    Key.opt("asset-static-info", bytes(AssetStaticInfoPrefix, asset.id.arr), readAssetStaticInfo, writeAssetStaticInfo)

  def nftCount(addressId: BigInt): Key[Int] =
    Key("nft-count", addr(50, addressId), Option(_).fold(0)(Ints.fromByteArray), Ints.toByteArray)

  val NFTPossessionPrefix: Short = 51
  def nftAt(addressId: BigInt, index: Int, assetId: IssuedAsset): Key[Option[Unit]] =
    Key.opt("nft-possession", bytes(NFTPossessionPrefix, addressId.toByteArray ++ Longs.toByteArray(index) ++ assetId.id.arr), _ => (), _ => Array.emptyByteArray)
}
