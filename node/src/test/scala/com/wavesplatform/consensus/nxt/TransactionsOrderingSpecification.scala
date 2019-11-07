package com.wavesplatform.consensus.nxt

import com.wavesplatform.account.{KeyPair, Address}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.TransactionsOrdering
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer._
import org.scalatest.{Assertions, Matchers, PropSpec}

import scala.util.Random

class TransactionsOrderingSpecification extends PropSpec with Assertions with Matchers {

  property("TransactionsOrdering.InBlock should sort correctly") {
    val correctSeq = Seq(
      TransferTransaction
        .selfSigned(1.toByte, KeyPair(Array.fill(32)(0: Byte)), Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(), Waves, 100000, Waves, 125L, Array.empty, 1)
        .right
        .get,
      TransferTransaction
        .selfSigned(1.toByte, KeyPair(Array.fill(32)(0: Byte)), Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(), Waves, 100000, Waves, 124L, Array.empty, 2)
        .right
        .get,
      TransferTransaction
        .selfSigned(1.toByte, KeyPair(Array.fill(32)(0: Byte)), Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(), Waves, 100000, Waves, 124L, Array.empty, 1)
        .right
        .get,
      TransferTransaction
        .selfSigned(1.toByte, KeyPair(Array.fill(32)(0: Byte)), Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(), Waves, 100000, Asset.fromCompatId(Some(ByteStr.empty)), 124L, Array.empty, 2)
        .right
        .get,
      TransferTransaction
        .selfSigned(1.toByte, KeyPair(Array.fill(32)(0: Byte)), Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(), Waves, 100000, Asset.fromCompatId(Some(ByteStr.empty)), 124L, Array.empty, 1)
        .right
        .get
    )

    val sorted = Random.shuffle(correctSeq).sorted(TransactionsOrdering.InBlock)

    sorted shouldBe correctSeq
  }

  property("TransactionsOrdering.InUTXPool should sort correctly") {
    val correctSeq = Seq(
      TransferTransaction
        .selfSigned(1.toByte, KeyPair(Array.fill(32)(0: Byte)), Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(), Waves, 100000, Waves, 124L, Array.empty, 1)
        .right
        .get,
      TransferTransaction
        .selfSigned(1.toByte, KeyPair(Array.fill(32)(0: Byte)), Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(), Waves, 100000, Waves, 123L, Array.empty, 1)
        .right
        .get,
      TransferTransaction
        .selfSigned(1.toByte, KeyPair(Array.fill(32)(0: Byte)), Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(), Waves, 100000, Waves, 123L, Array.empty, 2)
        .right
        .get,
      TransferTransaction
        .selfSigned(1.toByte, KeyPair(Array.fill(32)(0: Byte)), Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(), Waves, 100000, Asset.fromCompatId(Some(ByteStr.empty)), 124L, Array.empty, 1)
        .right
        .get,
      TransferTransaction
        .selfSigned(1.toByte, KeyPair(Array.fill(32)(0: Byte)), Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(), Waves, 100000, Asset.fromCompatId(Some(ByteStr.empty)), 124L, Array.empty, 2)
        .right
        .get
    )

    val sorted = Random.shuffle(correctSeq).sorted(TransactionsOrdering.InUTXPool)

    sorted shouldBe correctSeq
  }

  property("TransactionsOrdering.InBlock should sort txs by decreasing block timestamp") {
    val correctSeq = Seq(
      TransferTransaction
        .selfSigned(1.toByte, KeyPair(Array.fill(32)(0: Byte)), Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(), Waves, 100000, Waves, 1, Array(), 124L)
        .right
        .get,
      TransferTransaction
        .selfSigned(1.toByte, KeyPair(Array.fill(32)(0: Byte)), Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(), Waves, 100000, Waves, 1, Array(), 123L)
        .right
        .get
    )

    Random.shuffle(correctSeq).sorted(TransactionsOrdering.InBlock) shouldBe correctSeq
  }

  property("TransactionsOrdering.InUTXPool should sort txs by ascending block timestamp") {
    val correctSeq = Seq(
      TransferTransaction
        .selfSigned(1.toByte, KeyPair(Array.fill(32)(0: Byte)), Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(), Waves, 100000, Waves, 1, Array(), 123L)
        .right
        .get,
      TransferTransaction
        .selfSigned(1.toByte, KeyPair(Array.fill(32)(0: Byte)), Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet(), Waves, 100000, Waves, 1, Array(), 124L)
        .right
        .get
    )
    Random.shuffle(correctSeq).sorted(TransactionsOrdering.InUTXPool) shouldBe correctSeq
  }
}
