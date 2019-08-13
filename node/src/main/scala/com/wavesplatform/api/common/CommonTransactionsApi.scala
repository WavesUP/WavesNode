package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction.VanillaTransaction
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.state.diffs.FeeValidation.FeeDetails
import com.wavesplatform.state.{Blockchain, Height}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import monix.reactive.Observable

import scala.concurrent.Future

private[api] class CommonTransactionsApi(
    blockchain: Blockchain,
    utx: UtxPool,
    wallet: Wallet,
    publishTransaction: VanillaTransaction => Future[Either[ValidationError, VanillaTransaction]]
) {
  def transactionsByAddress(address: Address, fromId: Option[ByteStr] = None): Observable[(Height, VanillaTransaction)] =
    blockchain.addressTransactionsObservable(address, Set.empty, fromId)

  def transactionById(transactionId: ByteStr): Option[(Int, VanillaTransaction)] =
    blockchain.transactionInfo(transactionId)

  def unconfirmedTransactions(): Seq[VanillaTransaction] =
    utx.all

  def unconfirmedTransactionById(transactionId: ByteStr): Option[VanillaTransaction] =
    utx.transactionById(transactionId)

  def calculateFee(tx: VanillaTransaction): Either[ValidationError, (Asset, Long, Long)] =
    FeeValidation
      .getMinFee(blockchain, blockchain.height, tx)
      .map {
        case FeeDetails(asset, _, feeInAsset, feeInWaves) =>
          (asset, feeInAsset, feeInWaves)
      }

  def broadcastTransaction(tx: VanillaTransaction): Future[Either[ValidationError, VanillaTransaction]] = publishTransaction(tx)
}
