package com.wavesplatform.protobuf.transaction

import com.google.common.primitives.Bytes
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressOrAlias, AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.ScriptReader
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction.Attachment.Attachment.{BinaryValue, BoolValue, IntValue, StringValue}
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.serialization.Deser
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, EmptyDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.UpdateAssetInfoTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.{Proofs, TxValidationError}
import com.wavesplatform.utils.StringBytes
import com.wavesplatform.{transaction => vt}

import scala.util.Try
import com.wavesplatform.common.utils.EitherExt2

object PBTransactions {
  import com.wavesplatform.protobuf.utils.PBImplicitConversions._

  def create(
      sender: com.wavesplatform.account.PublicKey = PublicKey.empty,
      chainId: Byte = 0,
      fee: Long = 0L,
      feeAssetId: VanillaAssetId = Waves,
      timestamp: Long = 0L,
      version: Int = 0,
      proofsArray: Seq[com.wavesplatform.common.state.ByteStr] = Nil,
      data: com.wavesplatform.protobuf.transaction.Transaction.Data = com.wavesplatform.protobuf.transaction.Transaction.Data.Empty
  ): SignedTransaction = {
    new SignedTransaction(
      Some(Transaction(AddressScheme.current.chainId, sender: ByteStr, Some((feeAssetId, fee): Amount), timestamp, version, data)),
      proofsArray.map(bs => ByteString.copyFrom(bs.arr))
    )
  }

  def vanillaUnsafe(signedTx: PBSignedTransaction): VanillaTransaction = {
    import com.wavesplatform.common.utils._
    vanilla(signedTx, unsafe = true).explicitGet()
  }

  def tryToVanilla(signedTx: PBSignedTransaction): Try[VanillaTransaction] =
    vanilla(signedTx).left.map(err => new Exception(err.toString)).toTry

  def vanilla(signedTx: PBSignedTransaction, unsafe: Boolean = false): Either[ValidationError, VanillaTransaction] = {
    for {
      parsedTx <- signedTx.transaction.toRight(GenericError("Transaction must be specified"))
      fee      <- parsedTx.fee.toRight(GenericError("Fee must be specified"))
      _        <- Either.cond(parsedTx.data.isDefined, (), GenericError("Transaction data must be specified"))
      feeAmount = PBAmounts.toAssetAndAmount(fee)
      sender    = PublicKey(parsedTx.senderPublicKey.toByteArray)
      tx <- if (unsafe)
        Right(
          createVanillaUnsafe(
            parsedTx.version,
            parsedTx.chainId.toByte,
            sender,
            feeAmount._2,
            feeAmount._1,
            parsedTx.timestamp,
            Proofs(signedTx.proofs.map(bs => ByteStr(bs.toByteArray))),
            parsedTx.data
          )
        )
      else
        createVanilla(
          parsedTx.version,
          parsedTx.chainId.toByte,
          sender,
          feeAmount._2,
          feeAmount._1,
          parsedTx.timestamp,
          Proofs(signedTx.proofs.map(bs => ByteStr(bs.toByteArray))),
          parsedTx.data
        )
    } yield tx
  }

  private[this] def createVanilla(
      version: Int,
      chainId: Byte,
      sender: PublicKey,
      feeAmount: Long,
      feeAssetId: VanillaAssetId,
      timestamp: Long,
      proofs: Proofs,
      data: PBTransaction.Data
  ): Either[ValidationError, VanillaTransaction] = {

    val signature = proofs.toSignature
    val result: Either[ValidationError, VanillaTransaction] = data match {
      case Data.Genesis(GenesisTransactionData(recipient, amount)) =>
        for {
          addr <- PBRecipients.toAddress(recipient)
          tx   <- vt.GenesisTransaction.create(addr, amount, timestamp)
        } yield tx

      case Data.Payment(PaymentTransactionData(recipient, amount)) =>
        for {
          addr <- PBRecipients.toAddress(recipient)
          tx   <- vt.PaymentTransaction.create(sender, Address.fromBytes(recipient.toByteArray).explicitGet(), amount, feeAmount, timestamp, signature)
        } yield tx

      case Data.Transfer(TransferTransactionData(Some(recipient), Some(amount), attachment)) =>
        for {
          address <- recipient.toAddressOrAlias
        } yield vt.transfer.TransferTransaction(
          version.toByte,
          sender,
          address,
          amount.vanillaAssetId,
          amount.longAmount,
          feeAssetId,
          feeAmount,
          toVanillaAttachment(attachment),
          timestamp,
          proofs
        )

      case Data.CreateAlias(CreateAliasTransactionData(alias)) =>
        for {
          alias <- com.wavesplatform.account.Alias.createWithChainId(alias, chainId)
          tx    <- vt.CreateAliasTransaction.create(version.toByte, sender, alias, feeAmount, timestamp, Proofs(Seq(signature)))
        } yield tx

      case Data.Issue(IssueTransactionData(name, description, quantity, decimals, reissuable, script)) =>
        vt.assets.IssueTransaction.create(
          version.toByte,
          sender,
          name,
          description,
          quantity,
          decimals.toByte,
          reissuable,
          script.map(toVanillaScript),
          feeAmount,
          timestamp,
          proofs
        )

      case Data.Reissue(ReissueTransactionData(Some(Amount(assetId, amount)), reissuable)) =>
        vt.assets.ReissueTransaction.create(version.toByte, sender, IssuedAsset(assetId), amount, reissuable, feeAmount, timestamp, proofs)

      case Data.Burn(BurnTransactionData(Some(Amount(assetId, amount)))) =>
        vt.assets.BurnTransaction.create(version.toByte, sender, IssuedAsset(assetId), amount, feeAmount, timestamp, proofs)

      case Data.SetAssetScript(SetAssetScriptTransactionData(assetId, script)) =>
        vt.assets.SetAssetScriptTransaction.create(
          version.toByte,
          sender,
          IssuedAsset(assetId),
          script.map(toVanillaScript),
          feeAmount,
          timestamp,
          proofs
        )

      case Data.SetScript(SetScriptTransactionData(script)) =>
        vt.smart.SetScriptTransaction.create(
          version.toByte,
          sender,
          script.map(toVanillaScript),
          feeAmount,
          timestamp,
          proofs
        )

      case Data.Lease(LeaseTransactionData(Some(recipient), amount)) =>
        for {
          address <- recipient.toAddressOrAlias
          tx      <- vt.lease.LeaseTransaction.create(version.toByte, sender, address, amount, feeAmount, timestamp, proofs)
        } yield tx

      case Data.LeaseCancel(LeaseCancelTransactionData(leaseId)) =>
        vt.lease.LeaseCancelTransaction.create(version.toByte, sender, leaseId.toByteArray, feeAmount, timestamp, proofs)

      case Data.Exchange(ExchangeTransactionData(amount, price, buyMatcherFee, sellMatcherFee, Seq(buyOrder, sellOrder))) =>
        vt.assets.exchange.ExchangeTransaction.create(
          version.toByte,
          PBOrders.vanilla(buyOrder),
          PBOrders.vanilla(sellOrder),
          amount,
          price,
          buyMatcherFee,
          sellMatcherFee,
          feeAmount,
          timestamp,
          proofs
        )

      case Data.DataTransaction(dt) =>
        vt.DataTransaction.create(version.toByte, sender, dt.data.toList.map(toVanillaDataEntry), feeAmount, timestamp, proofs)

      case Data.MassTransfer(mt) =>
        vt.transfer.MassTransferTransaction.create(
          version.toByte,
          sender,
          PBAmounts.toVanillaAssetId(mt.assetId),
          mt.transfers.flatMap(t => t.getAddress.toAddressOrAlias.toOption.map(ParsedTransfer(_, t.amount))).toList,
          feeAmount,
          timestamp,
          toVanillaAttachment(mt.attachment),
          proofs
        )

      case Data.SponsorFee(SponsorFeeTransactionData(Some(Amount(assetId, minFee)))) =>
        vt.assets.SponsorFeeTransaction.create(
          version.toByte,
          sender,
          IssuedAsset(assetId),
          Option(minFee).filter(_ > 0),
          feeAmount,
          timestamp,
          proofs
        )

      case Data.InvokeScript(InvokeScriptTransactionData(Some(dappAddress), functionCall, payments)) =>
        import com.wavesplatform.common.utils._
        import com.wavesplatform.lang.v1.Serde
        import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL

        for {
          dApp <- PBRecipients.toAddressOrAlias(dappAddress)

          desFCOpt = Deser.parseOption(functionCall.asReadOnlyByteBuffer())(Serde.deserialize)

          _ <- Either.cond(
            desFCOpt.isEmpty || desFCOpt.get.isRight,
            (),
            GenericError(s"Invalid InvokeScript function call: ${desFCOpt.get.left.get}")
          )

          fcOpt = desFCOpt.map(_.explicitGet())

          _ <- Either.cond(fcOpt.isEmpty || fcOpt.exists(_.isInstanceOf[FUNCTION_CALL]), (), GenericError(s"Not a function call: $fcOpt"))

          tx <- vt.smart.InvokeScriptTransaction.create(
            version.toByte,
            sender,
            dApp,
            fcOpt.map(_.asInstanceOf[FUNCTION_CALL]),
            payments.map(p => vt.smart.InvokeScriptTransaction.Payment(p.longAmount, PBAmounts.toVanillaAssetId(p.assetId))),
            feeAmount,
            feeAssetId,
            timestamp,
            proofs
          )
        } yield tx

      case Data.UpdateAssetInfo(UpdateAssetInfoTransactionData(assetId, name, description)) =>
        UpdateAssetInfoTransaction.create(
          version.toByte,
          chainId,
          sender,
          assetId,
          name,
          description,
          timestamp,
          feeAmount,
          feeAssetId,
          proofs
        )

      case _ =>
        Left(TxValidationError.UnsupportedTransactionType)
    }

    result
  }

  private[this] def createVanillaUnsafe(
      version: Int,
      chainId: Byte,
      sender: PublicKey,
      feeAmount: Long,
      feeAssetId: VanillaAssetId,
      timestamp: Long,
      proofs: Proofs,
      data: PBTransaction.Data
  ): VanillaTransaction = {
    import com.wavesplatform.common.utils._

    val signature = proofs.toSignature
    data match {
      case Data.Genesis(GenesisTransactionData(recipient, amount)) =>
        vt.GenesisTransaction(PBRecipients.toAddress(recipient).explicitGet(), amount, timestamp, signature)

      case Data.Payment(PaymentTransactionData(recipient, amount)) =>
        vt.PaymentTransaction(sender, PBRecipients.toAddress(recipient).explicitGet(), amount, feeAmount, timestamp, signature)

      case Data.Transfer(TransferTransactionData(Some(recipient), Some(amount), attachment)) =>
        vt.transfer.TransferTransaction(
          version.toByte,
          sender,
          recipient.toAddressOrAlias.explicitGet(),
          amount.vanillaAssetId,
          amount.longAmount,
          feeAssetId,
          feeAmount,
          toVanillaAttachment(attachment),
          timestamp,
          proofs
        )

      case Data.CreateAlias(CreateAliasTransactionData(alias)) =>
        vt.CreateAliasTransaction(
          version.toByte,
          sender,
          com.wavesplatform.account.Alias.createWithChainId(alias, chainId).explicitGet(),
          feeAmount,
          timestamp,
          Proofs(signature)
        )

      case Data.Issue(IssueTransactionData(name, description, quantity, decimals, reissuable, script)) =>
        vt.assets.IssueTransaction(
          version.toByte,
          sender,
          name.toByteString,
          description.toByteString,
          quantity,
          decimals.toByte,
          reissuable,
          script.map(toVanillaScript),
          feeAmount,
          timestamp,
          proofs
        )

      case Data.Reissue(ReissueTransactionData(Some(Amount(assetId, amount)), reissuable)) =>
        vt.assets.ReissueTransaction(version.toByte, sender, IssuedAsset(assetId), amount, reissuable, feeAmount, timestamp, proofs)

      case Data.Burn(BurnTransactionData(Some(Amount(assetId, amount)))) =>
        vt.assets.BurnTransaction(version.toByte, sender, IssuedAsset(assetId), amount, feeAmount, timestamp, proofs)

      case Data.SetAssetScript(SetAssetScriptTransactionData(assetId, script)) =>
        vt.assets.SetAssetScriptTransaction(
          version.toByte,
          sender,
          IssuedAsset(assetId),
          script.map(toVanillaScript),
          feeAmount,
          timestamp,
          proofs
        )

      case Data.SetScript(SetScriptTransactionData(script)) =>
        vt.smart.SetScriptTransaction(
          version.toByte,
          sender,
          script.map(toVanillaScript),
          feeAmount,
          timestamp,
          proofs
        )

      case Data.Lease(LeaseTransactionData(Some(recipient), amount)) =>
        vt.lease.LeaseTransaction(version.toByte, sender, recipient.toAddressOrAlias.explicitGet(), amount, feeAmount, timestamp, proofs)

      case Data.LeaseCancel(LeaseCancelTransactionData(leaseId)) =>
        vt.lease.LeaseCancelTransaction(version.toByte, sender, leaseId.toByteArray, feeAmount, timestamp, proofs)

      case Data.Exchange(ExchangeTransactionData(amount, price, buyMatcherFee, sellMatcherFee, Seq(buyOrder, sellOrder))) =>
        vt.assets.exchange.ExchangeTransaction(
          version.toByte,
          PBOrders.vanilla(buyOrder),
          PBOrders.vanilla(sellOrder),
          amount,
          price,
          buyMatcherFee,
          sellMatcherFee,
          feeAmount,
          timestamp,
          proofs
        )

      case Data.DataTransaction(dt) =>
        vt.DataTransaction(version.toByte, sender, dt.data.toList.map(toVanillaDataEntry), feeAmount, timestamp, proofs)

      case Data.MassTransfer(mt) =>
        vt.transfer.MassTransferTransaction(
          version.toByte,
          sender,
          PBAmounts.toVanillaAssetId(mt.assetId),
          mt.transfers.flatMap(t => t.getAddress.toAddressOrAlias.toOption.map(ParsedTransfer(_, t.amount))).toList,
          feeAmount,
          timestamp,
          toVanillaAttachment(mt.attachment),
          proofs
        )

      case Data.SponsorFee(SponsorFeeTransactionData(Some(Amount(assetId, minFee)))) =>
        vt.assets.SponsorFeeTransaction(version.toByte, sender, IssuedAsset(assetId), Option(minFee).filter(_ > 0), feeAmount, timestamp, proofs)

      case Data.InvokeScript(InvokeScriptTransactionData(Some(dappAddress), functionCall, payments)) =>
        import com.wavesplatform.lang.v1.Serde
        import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL

        vt.smart.InvokeScriptTransaction(
          version.toByte,
          sender,
          PBRecipients.toAddressOrAlias(dappAddress).explicitGet(),
          Deser
            .parseOption(functionCall.asReadOnlyByteBuffer())(Serde.deserialize)
            .map(_.explicitGet().asInstanceOf[FUNCTION_CALL]),
          payments.map(p => vt.smart.InvokeScriptTransaction.Payment(p.longAmount, PBAmounts.toVanillaAssetId(p.assetId))),
          feeAmount,
          feeAssetId,
          timestamp,
          proofs
        )

      case other =>
        throw new IllegalArgumentException(s"Unsupported transaction data: $other")
    }
  }

  def protobuf(tx: VanillaTransaction): PBSignedTransaction = {
    val chainId = tx.chainByte

    tx match {
      case vt.GenesisTransaction(recipient, amount, timestamp, signature) =>
        val data = GenesisTransactionData(PBRecipients.create(recipient).getPublicKeyHash, amount)
        PBTransactions.create(
          sender = PublicKey(Array.emptyByteArray),
          chainId = 0: Byte,
          timestamp = timestamp,
          version = 1,
          proofsArray = Seq(signature),
          data = Data.Genesis(data)
        )

      case vt.PaymentTransaction(sender, recipient, amount, fee, timestamp, signature) =>
        val data = PaymentTransactionData(PBRecipients.create(recipient).getPublicKeyHash, amount)
        PBTransactions.create(sender, chainId, fee, Waves, timestamp, 1, Seq(signature), Data.Payment(data))

      case tx: vt.transfer.TransferTransaction =>
        import tx._
        val data = TransferTransactionData(Some(recipient), Some((assetId, amount)), toPBAttachment(attachment))
        PBTransactions.create(sender, chainId, fee, feeAssetId, timestamp, version, proofs, Data.Transfer(data))

      case tx: vt.CreateAliasTransaction =>
        import tx._
        val data = CreateAliasTransactionData(alias.name)
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.CreateAlias(data))

      case tx: vt.assets.exchange.ExchangeTransaction =>
        import tx._
        val data = ExchangeTransactionData(
          amount,
          price,
          buyMatcherFee,
          sellMatcherFee,
          Seq(PBOrders.protobuf(buyOrder), PBOrders.protobuf(sellOrder))
        )
        PBTransactions.create(tx.sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.Exchange(data))

      case tx: vt.assets.IssueTransaction =>
        import tx._
        val data = IssueTransactionData(name.toStringUtf8, description.toStringUtf8, quantity, decimals, reissuable, script.map(toPBScript))
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.Issue(data))

      case tx: vt.assets.ReissueTransaction =>
        import tx._
        val data = ReissueTransactionData(Some(Amount(asset.id, quantity)), reissuable)
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.Reissue(data))

      case tx: vt.assets.BurnTransaction =>
        import tx._
        val data = BurnTransactionData(Some(Amount(asset.id, quantity)))
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.Burn(data))

      case tx @ vt.assets.SetAssetScriptTransaction(_, sender, assetId, script, fee, timestamp, proofs) =>
        val data = SetAssetScriptTransactionData(assetId.id, script.map(toPBScript))
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, tx.version, proofs, Data.SetAssetScript(data))

      case tx @ vt.smart.SetScriptTransaction(_, sender, script, fee, timestamp, proofs) =>
        val data = SetScriptTransactionData(script.map(toPBScript))
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, tx.version, proofs, Data.SetScript(data))

      case tx: vt.lease.LeaseTransaction =>
        import tx._
        val data = LeaseTransactionData(Some(recipient), amount)
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.Lease(data))

      case tx: vt.lease.LeaseCancelTransaction =>
        import tx._
        val data = LeaseCancelTransactionData(leaseId)
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.LeaseCancel(data))

      case tx @ MassTransferTransaction(version, sender, assetId, transfers, fee, timestamp, attachment, proofs) =>
        val data = MassTransferTransactionData(
          PBAmounts.toPBAssetId(assetId),
          transfers.map(pt => MassTransferTransactionData.Transfer(Some(pt.address), pt.amount)),
          toPBAttachment(attachment)
        )
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.MassTransfer(data))

      case tx @ vt.DataTransaction(version, sender, data, fee, timestamp, proofs) =>
        val txData = DataTransactionData(data.map(toPBDataEntry))
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.DataTransaction(txData))

      case tx @ vt.assets.SponsorFeeTransaction(version, sender, assetId, minSponsoredAssetFee, fee, timestamp, proofs) =>
        val data = SponsorFeeTransactionData(Some(Amount(assetId.id, minSponsoredAssetFee.getOrElse(0L))))
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.SponsorFee(data))

      case vt.smart.InvokeScriptTransaction(version, sender, dappAddress, fcOpt, payment, fee, feeAssetId, timestamp, proofs) =>
        val data = Data.InvokeScript(toPBInvokeScriptData(dappAddress, fcOpt, payment))
        PBTransactions.create(sender, chainId, fee, feeAssetId, timestamp, version, proofs, data)

      case tx @ vt.assets.UpdateAssetInfoTransaction(version, _, sender, assetId, name, description, timestamp, _, _, proofs) =>
        val (feeAsset, feeAmount) = tx.assetFee

        val data = UpdateAssetInfoTransactionData()
          .withAssetId(assetId.id)
          .withName(name)
          .withDescription(description)

        PBTransactions.create(sender, chainId, feeAmount, feeAsset, timestamp, version, proofs, Data.UpdateAssetInfo(data))

      case _ =>
        throw new IllegalArgumentException(s"Unsupported transaction: $tx")
    }
  }

  def toPBInvokeScriptData(dappAddress: AddressOrAlias, fcOpt: Option[FUNCTION_CALL], payment: Seq[Payment]): InvokeScriptTransactionData = {
    import com.wavesplatform.lang.v1.Serde

    InvokeScriptTransactionData(
      Some(PBRecipients.create(dappAddress)),
      ByteString.copyFrom(Deser.serializeOption(fcOpt)(Serde.serialize(_))),
      payment.map(p => (p.assetId, p.amount): Amount)
    )
  }

  def toVanillaDataEntry(de: DataTransactionData.DataEntry): com.wavesplatform.state.DataEntry[_] = {
    import DataTransactionData.DataEntry.{Value => DEV}

    de.value match {
      case DEV.IntValue(num)      => IntegerDataEntry(de.key, num)
      case DEV.BoolValue(bool)    => BooleanDataEntry(de.key, bool)
      case DEV.BinaryValue(bytes) => BinaryDataEntry(de.key, bytes.toByteArray)
      case DEV.StringValue(str)   => StringDataEntry(de.key, str)
      case DEV.Empty              => EmptyDataEntry(de.key)
    }
  }

  def toPBDataEntry(de: com.wavesplatform.state.DataEntry[_]): DataTransactionData.DataEntry = {
    DataTransactionData.DataEntry(
      de.key,
      de match {
        case IntegerDataEntry(_, value) => DataTransactionData.DataEntry.Value.IntValue(value)
        case BooleanDataEntry(_, value) => DataTransactionData.DataEntry.Value.BoolValue(value)
        case BinaryDataEntry(_, value)  => DataTransactionData.DataEntry.Value.BinaryValue(value)
        case StringDataEntry(_, value)  => DataTransactionData.DataEntry.Value.StringValue(value)
        case EmptyDataEntry(_)          => DataTransactionData.DataEntry.Value.Empty
      }
    )
  }

  def toVanillaAttachment(attachment: Option[Attachment]): Option[vt.transfer.Attachment] =
    attachment.flatMap { a =>
      import Attachment.{Attachment => PBA}
      a.attachment match {
        case PBA.IntValue(value)    => Some(vt.transfer.Attachment.Num(value))
        case PBA.BoolValue(value)   => Some(vt.transfer.Attachment.Bool(value))
        case PBA.BinaryValue(value) => Some(vt.transfer.Attachment.Bin(value.toByteArray))
        case PBA.StringValue(value) => Some(vt.transfer.Attachment.Str(value))
        case _                      => None
      }
    }

  def toPBAttachment(attachment: Option[vt.transfer.Attachment]): Option[Attachment] = {
    import vt.transfer.{Attachment => VA}
    attachment
      .map {
        case VA.Num(value)  => IntValue(value)
        case VA.Bool(value) => BoolValue(value)
        case VA.Bin(value)  => BinaryValue(ByteString.copyFrom(value))
        case VA.Str(value)  => StringValue(value)
      }
      .map(Attachment.of)
  }

  def toVanillaScript(script: Script): com.wavesplatform.lang.script.Script = {
    import com.wavesplatform.common.utils._
    val array = Bytes.concat(Array(script.version.toByte), script.bytes.toByteArray)
    ScriptReader.fromBytes(array).explicitGet()
  }

  def toPBScript(script: com.wavesplatform.lang.script.Script): Script = {
    val Array(ver, body @ _*) = script.bytes().arr
    Script.of(ByteString.copyFrom(body.toArray), ver)
  }
}
