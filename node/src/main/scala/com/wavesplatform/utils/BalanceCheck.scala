package com.wavesplatform.utils

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils._
import com.wavesplatform.state.Blockchain

import scala.collection.concurrent.TrieMap

object BalanceCheck extends ScorexLogging {
  private[this] val map = TrieMap.empty[Address, Long]

  def checkBalance(blockchain: Blockchain, address: String): Unit = {
    val addressValue = Address.fromString(address).explicitGet()
    val balance = blockchain.balance(addressValue)
    map.get(addressValue) match {
      case Some(`balance`) => // Ignore
      case _ =>
        log.debug(s"Balance of $address on ${blockchain.height} is $balance")
        map(addressValue) = balance
    }
  }
}
