package com.wavesplatform.metrics

import com.google.common.base.CaseFormat
import com.wavesplatform.transaction.TransactionParsers
import kamon.Kamon
import kamon.metric.Timer
import supertagged._

object TxProcessingStats {
  val typeToName: Map[Byte, String] = {
    val converter = CaseFormat.UPPER_CAMEL.converterTo(CaseFormat.LOWER_HYPHEN)

    TransactionParsers.byName.map {
      case (name, builder) =>
        val typeName = converter.convert(
          name
            .replace("V1", "")
            .replace("V2", "")
            .replace("Transaction", "")
        )

        (builder.typeId, typeName)
    }
  }

  object TxTimer extends TaggedType[Timer]

  type TxTimer = TxTimer.Type

  implicit class TxTimerExt(val t: TxTimer) extends AnyVal {
    def measureForType[A](typeId: Byte)(f: => A): A = {
      val start  = t.withTag("transaction-type", typeToName(typeId)).start()
      val result = f
      start.stop()
      result
    }
  }

  val invokedScriptExecution: TxTimer    = TxTimer(Kamon.timer("tx.processing.script-execution.invoked").withoutTags())
  val accountScriptExecution: TxTimer    = TxTimer(Kamon.timer("tx.processing.script-execution.account").withoutTags())
  val assetScriptExecution: TxTimer      = TxTimer(Kamon.timer("tx.processing.script-execution.asset").withoutTags())
  val signatureVerification: TxTimer     = TxTimer(Kamon.timer("tx.processing.validation.signature").withoutTags())
  val balanceValidation: TxTimer         = TxTimer(Kamon.timer("tx.processing.validation.balance").withoutTags())
  val commonValidation: TxTimer          = TxTimer(Kamon.timer("tx.processing.validation.common").withoutTags())
  val transactionDiffValidation: TxTimer = TxTimer(Kamon.timer("tx.processing.validation.diff").withoutTags())
  val orderValidation: TxTimer           = TxTimer(Kamon.timer("tx.processing.validation.order").withoutTags())
}
