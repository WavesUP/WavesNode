package com.wavesplatform.network

import java.util

import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.MessageToMessageCodec

import scala.util.{Failure, Success}

@Sharable
class MessageCodec(peerDatabase: PeerDatabase) extends MessageToMessageCodec[RawBytes, Message] with ScorexLogging {

  import BasicMessagesRepo.specsByCodes

  override def encode(ctx: ChannelHandlerContext, msg: Message, out: util.List[AnyRef]): Unit = msg match {
    // Have no spec
    case r: RawBytes              => out.add(r)
    case LocalScoreChanged(score) => out.add(RawBytes(ScoreSpec.messageCode, ScoreSpec.serializeData(score)))
    case BlockForged(b)           => out.add(RawBytes.fromBlock(b))

    // With a spec
    case GetPeers              => out.add(RawBytes(GetPeersSpec.messageCode, Array[Byte]()))
    case k: KnownPeers         => out.add(RawBytes(PeersSpec.messageCode, PeersSpec.serializeData(k)))
    case gs: GetSignatures     => out.add(RawBytes(GetSignaturesSpec.messageCode, GetSignaturesSpec.serializeData(gs)))
    case s: Signatures         => out.add(RawBytes(SignaturesSpec.messageCode, SignaturesSpec.serializeData(s)))
    case g: GetBlock           => out.add(RawBytes(GetBlockSpec.messageCode, GetBlockSpec.serializeData(g)))
    case m: MicroBlockInv      => out.add(RawBytes(MicroBlockInvSpec.messageCode, MicroBlockInvSpec.serializeData(m)))
    case m: MicroBlockRequest  => out.add(RawBytes(MicroBlockRequestSpec.messageCode, MicroBlockRequestSpec.serializeData(m)))
    case MicroBlockResponse(m) => out.add(RawBytes.fromMicroBlock(m))
  }

  override def decode(ctx: ChannelHandlerContext, msg: RawBytes, out: util.List[AnyRef]): Unit = {
    specsByCodes.get(msg.code) match {
      case Some(spec) =>
        spec.deserializeData(msg.data) match {
          case Success(x) => out.add(x)
          case Failure(e) => block(ctx, e)
        }

      case None =>
        block(ctx, new IllegalArgumentException(s"Unknown message: $msg"))
    }
  }

  protected def block(ctx: ChannelHandlerContext, e: Throwable): Unit = {
    peerDatabase.blacklistAndClose(ctx.channel(), s"Invalid message. ${e.getMessage}")
  }

}
