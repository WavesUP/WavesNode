package com.wavesplatform.it.sync.transactions

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.Node
import com.wavesplatform.it.sync.calcDataFee
import com.wavesplatform.state.BinaryDataEntry

import scala.concurrent.duration._

trait OverflowBlock {
  protected def sender: Node

  // Hack to bypass instant micro mining
  def overflowBlock(): Unit = {
    import com.wavesplatform.it.api.SyncHttpApi._
    val entries = List.tabulate(4)(n => BinaryDataEntry("test" + n, ByteStr(Array.fill(32767)(n.toByte))))
    val addr    = sender.createAddress()
    val fee     = calcDataFee(entries, 1)
    val height = sender.height
    sender.waitForHeight(height)
    sender.transfer(sender.address, addr, fee * 10, waitForTx = true)
    for (_ <- 1 to 7) sender.putData(addr, entries, fee)
    sender.waitFor("empty utx")(n => n.utxSize, (utxSize: Int) => utxSize == 0, 100.millis)
  }
}
