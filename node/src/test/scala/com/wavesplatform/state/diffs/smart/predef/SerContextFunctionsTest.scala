package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer.{Attachment, TransferTransaction}
import com.wavesplatform.transaction.{DataTransaction, Proofs}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import scorex.crypto.encode.Base64

class SerContextFunctionsTest extends PropSpec with PropertyChecks with Matchers with NoShrink with TransactionGen {
  property("check serialization of script with all functions") {
    val entry1 = IntegerDataEntry("int", 24)
    val entry2 = BooleanDataEntry("bool", true)
    val entry3 = BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=")))
    val entry4 = StringDataEntry("str", "test")

    val dtx = DataTransaction
      .create(
        1.toByte,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").right.get,
        List(entry1, entry2, entry3, entry4),
        100000,
        1526911531530L,
        Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .right
      .get

    val ttx =
      TransferTransaction(
        2.toByte,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").right.get,
        Address.fromString("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8").right.get,
        Waves,
        100000000,
        Waves,
        100000000,
        Attachment.fromBytes(Base58.tryDecodeWithLimit("4t2Xazb2SX").get),
        1526641218066L,
        Proofs(Seq(ByteStr.decodeBase58("4bfDaqBcnK3hT8ywFEFndxtS1DTSYfncUqd4s5Vyaa66PZHawtC73rDswUur6QZu5RpqM7L9NFgBHT1vhCoox4vi").get))
      )

    val untypedScript  = Parser.parseExpr(scriptWithAllV1Functions(dtx, ttx)).get.value
    val compiledScript = ExpressionCompiler(compilerContext(V1, Expression, isAssetScript = false), untypedScript).explicitGet()._1
    val bytes = Array[Byte](4, 0, 0, 0, 3, 114, 110, 100, 9, 0, 0, 0, 0, 0, 0, 2, 9, 0, 0, 106, 0, 0, 0, 2, 8, 5, 0, 0, 0, 2, 116, 120, 0, 0, 0, 9,
      116, 105, 109, 101, 115, 116, 97, 109, 112, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 7, 108, 111, 110, 103, 65, 108,
      108, 3, 3, 3, 3, 9, 0, 0, 0, 0, 0, 0, 2, 9, 0, 0, 104, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 3, -24, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0,
      7, -48, 9, 0, 0, 0, 0, 0, 0, 2, 9, 0, 0, 105, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 3, -24, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 1, -12,
      7, 9, 0, 0, 0, 0, 0, 0, 2, 9, 0, 0, 106, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 3, -24, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 9, 0,
      0, 0, 0, 0, 0, 2, 9, 0, 0, 100, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 3, -24, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 3, -22, 7, 9, 0, 0, 0,
      0, 0, 0, 2, 9, 0, 0, 101, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 3, -24, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 3, -26, 7, 4, 0, 0, 0, 9,
      115, 117, 109, 83, 116, 114, 105, 110, 103, 9, 0, 0, 0, 0, 0, 0, 2, 9, 0, 1, 44, 0, 0, 0, 2, 9, 0, 1, 44, 0, 0, 0, 2, 2, 0, 0, 0, 2, 104, 97, 2,
      0, 0, 0, 1, 45, 2, 0, 0, 0, 2, 104, 97, 2, 0, 0, 0, 5, 104, 97, 45, 104, 97, 4, 0, 0, 0, 13, 115, 117, 109, 66, 121, 116, 101, 86, 101, 99, 116,
      111, 114, 4, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104, 48, 5, 0, 0, 0, 2, 116, 120, 3, 9, 0, 0, 1, 0, 0, 0, 2, 5, 0, 0, 0, 7, 36, 109, 97, 116, 99,
      104, 48, 2, 0, 0, 0, 15, 68, 97, 116, 97, 84, 114, 97, 110, 115, 97, 99, 116, 105, 111, 110, 4, 0, 0, 0, 2, 100, 48, 5, 0, 0, 0, 7, 36, 109, 97,
      116, 99, 104, 48, 4, 0, 0, 0, 4, 98, 111, 100, 121, 8, 5, 0, 0, 0, 2, 100, 48, 0, 0, 0, 9, 98, 111, 100, 121, 66, 121, 116, 101, 115, 9, 0, 0,
      0, 0, 0, 0, 2, 9, 0, 0, -53, 0, 0, 0, 2, 5, 0, 0, 0, 4, 98, 111, 100, 121, 1, 0, 0, 0, 100, 12, 1, -43, 40, -86, -66, -61, 92, -95, 0, -40, 124,
      123, 122, 18, -122, 50, -6, -15, -100, -44, 69, 49, -127, -108, 87, 68, 81, 19, -93, 42, 33, -17, 34, 0, 4, 0, 3, 105, 110, 116, 0, 0, 0, 0, 0,
      0, 0, 0, 24, 0, 4, 98, 111, 111, 108, 1, 1, 0, 4, 98, 108, 111, 98, 2, 0, 5, 97, 108, 105, 99, 101, 0, 3, 115, 116, 114, 3, 0, 4, 116, 101, 115,
      116, 0, 0, 1, 99, -125, 4, -6, 10, 0, 0, 0, 0, 0, 1, -122, -96, 9, 0, 0, -53, 0, 0, 0, 2, 1, 0, 0, 0, 100, 12, 1, -43, 40, -86, -66, -61, 92,
      -95, 0, -40, 124, 123, 122, 18, -122, 50, -6, -15, -100, -44, 69, 49, -127, -108, 87, 68, 81, 19, -93, 42, 33, -17, 34, 0, 4, 0, 3, 105, 110,
      116, 0, 0, 0, 0, 0, 0, 0, 0, 24, 0, 4, 98, 111, 111, 108, 1, 1, 0, 4, 98, 108, 111, 98, 2, 0, 5, 97, 108, 105, 99, 101, 0, 3, 115, 116, 114, 3,
      0, 4, 116, 101, 115, 116, 0, 0, 1, 99, -125, 4, -6, 10, 0, 0, 0, 0, 0, 1, -122, -96, 1, 0, 0, 0, 100, 12, 1, -43, 40, -86, -66, -61, 92, -95, 0,
      -40, 124, 123, 122, 18, -122, 50, -6, -15, -100, -44, 69, 49, -127, -108, 87, 68, 81, 19, -93, 42, 33, -17, 34, 0, 4, 0, 3, 105, 110, 116, 0, 0,
      0, 0, 0, 0, 0, 0, 24, 0, 4, 98, 111, 111, 108, 1, 1, 0, 4, 98, 108, 111, 98, 2, 0, 5, 97, 108, 105, 99, 101, 0, 3, 115, 116, 114, 3, 0, 4, 116,
      101, 115, 116, 0, 0, 1, 99, -125, 4, -6, 10, 0, 0, 0, 0, 0, 1, -122, -96, 3, 9, 0, 0, 1, 0, 0, 0, 2, 5, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104,
      48, 2, 0, 0, 0, 19, 84, 114, 97, 110, 115, 102, 101, 114, 84, 114, 97, 110, 115, 97, 99, 116, 105, 111, 110, 6, 7, 4, 0, 0, 0, 7, 101, 113, 85,
      110, 105, 111, 110, 4, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104, 48, 5, 0, 0, 0, 2, 116, 120, 3, 9, 0, 0, 1, 0, 0, 0, 2, 5, 0, 0, 0, 7, 36, 109,
      97, 116, 99, 104, 48, 2, 0, 0, 0, 15, 68, 97, 116, 97, 84, 114, 97, 110, 115, 97, 99, 116, 105, 111, 110, 6, 3, 9, 0, 0, 1, 0, 0, 0, 2, 5, 0, 0,
      0, 7, 36, 109, 97, 116, 99, 104, 48, 2, 0, 0, 0, 19, 84, 114, 97, 110, 115, 102, 101, 114, 84, 114, 97, 110, 115, 97, 99, 116, 105, 111, 110, 4,
      0, 0, 0, 2, 116, 48, 5, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104, 48, 9, 0, 0, 0, 0, 0, 0, 2, 8, 5, 0, 0, 0, 2, 116, 48, 0, 0, 0, 9, 114, 101, 99,
      105, 112, 105, 101, 110, 116, 9, 1, 0, 0, 0, 7, 65, 100, 100, 114, 101, 115, 115, 0, 0, 0, 1, 1, 0, 0, 0, 26, 1, 84, 100, 23, -115, -33, -128,
      -20, -97, 62, -33, -42, 86, -24, -22, 104, -110, -85, 40, -23, -25, 122, -18, -70, -100, -99, 7, 4, 0, 0, 0, 5, 98, 97, 115, 105, 99, 3, 3, 3,
      5, 0, 0, 0, 7, 108, 111, 110, 103, 65, 108, 108, 5, 0, 0, 0, 9, 115, 117, 109, 83, 116, 114, 105, 110, 103, 7, 5, 0, 0, 0, 13, 115, 117, 109,
      66, 121, 116, 101, 86, 101, 99, 116, 111, 114, 7, 5, 0, 0, 0, 7, 101, 113, 85, 110, 105, 111, 110, 7, 4, 0, 0, 0, 6, 110, 101, 80, 114, 105,
      109, 3, 3, 9, 1, 0, 0, 0, 2, 33, 61, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 3, -24, 0, 0, 0, 0, 0, 0, 0, 3, -25, 9, 1, 0, 0, 0, 2, 33, 61, 0, 0, 0, 2,
      9, 0, 1, 44, 0, 0, 0, 2, 2, 0, 0, 0, 2, 104, 97, 2, 0, 0, 0, 2, 104, 97, 2, 0, 0, 0, 5, 104, 97, 45, 104, 97, 7, 9, 1, 0, 0, 0, 2, 33, 61, 0, 0,
      0, 2, 8, 5, 0, 0, 0, 2, 116, 120, 0, 0, 0, 9, 98, 111, 100, 121, 66, 121, 116, 101, 115, 1, 0, 0, 0, 4, -123, -88, 90, -123, 7, 4, 0, 0, 0, 24,
      110, 101, 68, 97, 116, 97, 69, 110, 116, 114, 121, 65, 110, 100, 71, 101, 116, 69, 108, 101, 109, 101, 110, 116, 4, 0, 0, 0, 7, 36, 109, 97,
      116, 99, 104, 48, 5, 0, 0, 0, 2, 116, 120, 3, 9, 0, 0, 1, 0, 0, 0, 2, 5, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104, 48, 2, 0, 0, 0, 15, 68, 97, 116,
      97, 84, 114, 97, 110, 115, 97, 99, 116, 105, 111, 110, 4, 0, 0, 0, 2, 100, 49, 5, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104, 48, 9, 1, 0, 0, 0, 2,
      33, 61, 0, 0, 0, 2, 9, 0, 1, -111, 0, 0, 0, 2, 8, 5, 0, 0, 0, 2, 100, 49, 0, 0, 0, 4, 100, 97, 116, 97, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 1, 0, 0,
      0, 9, 68, 97, 116, 97, 69, 110, 116, 114, 121, 0, 0, 0, 2, 2, 0, 0, 0, 2, 104, 97, 6, 3, 9, 0, 0, 1, 0, 0, 0, 2, 5, 0, 0, 0, 7, 36, 109, 97,
      116, 99, 104, 48, 2, 0, 0, 0, 19, 84, 114, 97, 110, 115, 102, 101, 114, 84, 114, 97, 110, 115, 97, 99, 116, 105, 111, 110, 6, 7, 4, 0, 0, 0, 24,
      110, 101, 79, 112, 116, 105, 111, 110, 65, 110, 100, 69, 120, 116, 114, 97, 99, 116, 72, 101, 105, 103, 104, 116, 4, 0, 0, 0, 7, 36, 109, 97,
      116, 99, 104, 48, 5, 0, 0, 0, 2, 116, 120, 3, 9, 0, 0, 1, 0, 0, 0, 2, 5, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104, 48, 2, 0, 0, 0, 15, 68, 97, 116,
      97, 84, 114, 97, 110, 115, 97, 99, 116, 105, 111, 110, 6, 3, 9, 0, 0, 1, 0, 0, 0, 2, 5, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104, 48, 2, 0, 0, 0,
      19, 84, 114, 97, 110, 115, 102, 101, 114, 84, 114, 97, 110, 115, 97, 99, 116, 105, 111, 110, 9, 1, 0, 0, 0, 2, 33, 61, 0, 0, 0, 2, 9, 1, 0, 0,
      0, 7, 101, 120, 116, 114, 97, 99, 116, 0, 0, 0, 1, 9, 0, 3, -23, 0, 0, 0, 1, 8, 5, 0, 0, 0, 2, 116, 120, 0, 0, 0, 2, 105, 100, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 7, 4, 0, 0, 0, 2, 110, 101, 3, 3, 5, 0, 0, 0, 6, 110, 101, 80, 114, 105, 109, 5, 0, 0, 0, 24, 110, 101, 68, 97, 116, 97, 69, 110, 116,
      114, 121, 65, 110, 100, 71, 101, 116, 69, 108, 101, 109, 101, 110, 116, 7, 5, 0, 0, 0, 24, 110, 101, 79, 112, 116, 105, 111, 110, 65, 110, 100,
      69, 120, 116, 114, 97, 99, 116, 72, 101, 105, 103, 104, 116, 7, 4, 0, 0, 0, 7, 103, 116, 101, 76, 111, 110, 103, 3, 9, 0, 0, 102, 0, 0, 0, 2, 0,
      0, 0, 0, 0, 0, 0, 3, -24, 0, 0, 0, 0, 0, 0, 0, 3, -25, 9, 0, 0, 103, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 3, -24, 0, 0, 0, 0, 0, 0, 0, 3, -25, 7, 4,
      0, 0, 0, 11, 103, 101, 116, 76, 105, 115, 116, 83, 105, 122, 101, 4, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104, 48, 5, 0, 0, 0, 2, 116, 120, 3, 9,
      0, 0, 1, 0, 0, 0, 2, 5, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104, 48, 2, 0, 0, 0, 15, 68, 97, 116, 97, 84, 114, 97, 110, 115, 97, 99, 116, 105,
      111, 110, 4, 0, 0, 0, 2, 100, 50, 5, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104, 48, 9, 1, 0, 0, 0, 2, 33, 61, 0, 0, 0, 2, 9, 0, 1, -112, 0, 0, 0, 1,
      8, 5, 0, 0, 0, 2, 100, 50, 0, 0, 0, 4, 100, 97, 116, 97, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 9, 0, 0, 1, 0, 0, 0, 2, 5, 0, 0, 0, 7, 36, 109, 97, 116,
      99, 104, 48, 2, 0, 0, 0, 19, 84, 114, 97, 110, 115, 102, 101, 114, 84, 114, 97, 110, 115, 97, 99, 116, 105, 111, 110, 6, 7, 4, 0, 0, 0, 5, 117,
      110, 97, 114, 121, 3, 9, 0, 0, 0, 0, 0, 0, 2, 0, -1, -1, -1, -1, -1, -1, -1, -1, 0, -1, -1, -1, -1, -1, -1, -1, -1, 9, 0, 0, 0, 0, 0, 0, 2, 7,
      9, 1, 0, 0, 0, 1, 33, 0, 0, 0, 1, 6, 7, 4, 0, 0, 0, 8, 102, 114, 65, 99, 116, 105, 111, 110, 9, 0, 0, 0, 0, 0, 0, 2, 9, 0, 0, 107, 0, 0, 0, 3,
      0, 0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 9, 4, 0, 0, 0, 8, 98, 121, 116, 101,
      115, 79, 112, 115, 4, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104, 48, 5, 0, 0, 0, 2, 116, 120, 3, 9, 0, 0, 1, 0, 0, 0, 2, 5, 0, 0, 0, 7, 36, 109, 97,
      116, 99, 104, 48, 2, 0, 0, 0, 15, 68, 97, 116, 97, 84, 114, 97, 110, 115, 97, 99, 116, 105, 111, 110, 4, 0, 0, 0, 2, 100, 51, 5, 0, 0, 0, 7, 36,
      109, 97, 116, 99, 104, 48, 3, 3, 3, 3, 9, 1, 0, 0, 0, 2, 33, 61, 0, 0, 0, 2, 9, 0, 0, -56, 0, 0, 0, 1, 8, 5, 0, 0, 0, 2, 100, 51, 0, 0, 0, 9,
      98, 111, 100, 121, 66, 121, 116, 101, 115, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 1, 0, 0, 0, 2, 33, 61, 0, 0, 0, 2, 9, 0, 0, -55, 0, 0, 0, 2, 8, 5, 0,
      0, 0, 2, 100, 51, 0, 0, 0, 9, 98, 111, 100, 121, 66, 121, 116, 101, 115, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 2, 9, 49, 7, 9, 1, 0, 0, 0, 2,
      33, 61, 0, 0, 0, 2, 9, 0, 0, -54, 0, 0, 0, 2, 8, 5, 0, 0, 0, 2, 100, 51, 0, 0, 0, 9, 98, 111, 100, 121, 66, 121, 116, 101, 115, 0, 0, 0, 0, 0,
      0, 0, 0, 1, 1, 0, 0, 0, 2, 9, 49, 7, 9, 1, 0, 0, 0, 2, 33, 61, 0, 0, 0, 2, 9, 1, 0, 0, 0, 14, 116, 97, 107, 101, 82, 105, 103, 104, 116, 66,
      121, 116, 101, 115, 0, 0, 0, 2, 8, 5, 0, 0, 0, 2, 100, 51, 0, 0, 0, 9, 98, 111, 100, 121, 66, 121, 116, 101, 115, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
      0, 0, 0, 2, 9, 49, 7, 9, 1, 0, 0, 0, 2, 33, 61, 0, 0, 0, 2, 9, 1, 0, 0, 0, 14, 100, 114, 111, 112, 82, 105, 103, 104, 116, 66, 121, 116, 101,
      115, 0, 0, 0, 2, 8, 5, 0, 0, 0, 2, 100, 51, 0, 0, 0, 9, 98, 111, 100, 121, 66, 121, 116, 101, 115, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 2, 9,
      49, 7, 3, 9, 0, 0, 1, 0, 0, 0, 2, 5, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104, 48, 2, 0, 0, 0, 19, 84, 114, 97, 110, 115, 102, 101, 114, 84, 114,
      97, 110, 115, 97, 99, 116, 105, 111, 110, 4, 0, 0, 0, 2, 116, 49, 5, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104, 48, 9, 0, 0, 0, 0, 0, 0, 2, 9, 1, 0,
      0, 0, 9, 105, 115, 68, 101, 102, 105, 110, 101, 100, 0, 0, 0, 1, 8, 5, 0, 0, 0, 2, 116, 49, 0, 0, 0, 10, 102, 101, 101, 65, 115, 115, 101, 116,
      73, 100, 7, 7, 4, 0, 0, 0, 6, 115, 116, 114, 79, 112, 115, 3, 3, 3, 3, 9, 1, 0, 0, 0, 2, 33, 61, 0, 0, 0, 2, 9, 0, 1, 49, 0, 0, 0, 1, 2, 0, 0,
      0, 4, 104, 97, 104, 97, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 1, 0, 0, 0, 2, 33, 61, 0, 0, 0, 2, 9, 0, 1, 47, 0, 0, 0, 2, 2, 0, 0, 0, 4, 104, 97, 104,
      97, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0, 7, 9, 1, 0, 0, 0, 2, 33, 61, 0, 0, 0, 2, 9, 0, 1, 48, 0, 0, 0, 2, 2, 0, 0, 0, 4, 104, 97, 104, 97,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 7, 9, 1, 0, 0, 0, 2, 33, 61, 0, 0, 0, 2, 9, 1, 0, 0, 0, 9, 116, 97, 107, 101, 82, 105, 103, 104, 116,
      0, 0, 0, 2, 2, 0, 0, 0, 4, 104, 97, 104, 97, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0, 7, 9, 1, 0, 0, 0, 2, 33, 61, 0, 0, 0, 2, 9, 1, 0, 0, 0,
      9, 100, 114, 111, 112, 82, 105, 103, 104, 116, 0, 0, 0, 2, 2, 0, 0, 0, 4, 104, 97, 104, 97, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 7, 4, 0,
      0, 0, 4, 112, 117, 114, 101, 3, 3, 3, 3, 3, 3, 3, 5, 0, 0, 0, 5, 98, 97, 115, 105, 99, 5, 0, 0, 0, 2, 110, 101, 7, 5, 0, 0, 0, 7, 103, 116, 101,
      76, 111, 110, 103, 7, 5, 0, 0, 0, 11, 103, 101, 116, 76, 105, 115, 116, 83, 105, 122, 101, 7, 5, 0, 0, 0, 5, 117, 110, 97, 114, 121, 7, 5, 0, 0,
      0, 8, 102, 114, 65, 99, 116, 105, 111, 110, 7, 5, 0, 0, 0, 8, 98, 121, 116, 101, 115, 79, 112, 115, 7, 5, 0, 0, 0, 6, 115, 116, 114, 79, 112,
      115, 7, 4, 0, 0, 0, 6, 116, 120, 66, 121, 73, 100, 4, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104, 48, 5, 0, 0, 0, 2, 116, 120, 3, 9, 0, 0, 1, 0, 0,
      0, 2, 5, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104, 48, 2, 0, 0, 0, 15, 68, 97, 116, 97, 84, 114, 97, 110, 115, 97, 99, 116, 105, 111, 110, 6, 3, 9,
      0, 0, 1, 0, 0, 0, 2, 5, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104, 48, 2, 0, 0, 0, 19, 84, 114, 97, 110, 115, 102, 101, 114, 84, 114, 97, 110, 115,
      97, 99, 116, 105, 111, 110, 4, 0, 0, 0, 1, 103, 9, 1, 0, 0, 0, 7, 101, 120, 116, 114, 97, 99, 116, 0, 0, 0, 1, 9, 0, 3, -24, 0, 0, 0, 1, 1, 0,
      0, 0, 32, -127, -8, -79, -21, -18, 42, -48, -20, -34, -84, -89, 17, 125, -43, 82, 88, -78, -58, -94, -5, -31, 50, 36, 76, 53, 88, 86, 48, 93,
      -67, 20, 11, 9, 0, 0, 0, 0, 0, 0, 2, 8, 5, 0, 0, 0, 1, 103, 0, 0, 0, 2, 105, 100, 1, 0, 0, 0, 32, -127, -8, -79, -21, -18, 42, -48, -20, -34,
      -84, -89, 17, 125, -43, 82, 88, -78, -58, -94, -5, -31, 50, 36, 76, 53, 88, 86, 48, 93, -67, 20, 11, 7, 4, 0, 0, 0, 7, 101, 110, 116, 114, 105,
      101, 115, 4, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104, 48, 5, 0, 0, 0, 2, 116, 120, 3, 9, 0, 0, 1, 0, 0, 0, 2, 5, 0, 0, 0, 7, 36, 109, 97, 116, 99,
      104, 48, 2, 0, 0, 0, 15, 68, 97, 116, 97, 84, 114, 97, 110, 115, 97, 99, 116, 105, 111, 110, 4, 0, 0, 0, 1, 100, 5, 0, 0, 0, 7, 36, 109, 97,
      116, 99, 104, 48, 4, 0, 0, 0, 3, 105, 110, 116, 9, 1, 0, 0, 0, 7, 101, 120, 116, 114, 97, 99, 116, 0, 0, 0, 1, 9, 0, 4, 16, 0, 0, 0, 2, 8, 5, 0,
      0, 0, 1, 100, 0, 0, 0, 4, 100, 97, 116, 97, 2, 0, 0, 0, 3, 105, 110, 116, 4, 0, 0, 0, 4, 98, 111, 111, 108, 9, 1, 0, 0, 0, 7, 101, 120, 116,
      114, 97, 99, 116, 0, 0, 0, 1, 9, 0, 4, 17, 0, 0, 0, 2, 8, 5, 0, 0, 0, 1, 100, 0, 0, 0, 4, 100, 97, 116, 97, 2, 0, 0, 0, 4, 98, 111, 111, 108, 4,
      0, 0, 0, 4, 98, 108, 111, 98, 9, 1, 0, 0, 0, 7, 101, 120, 116, 114, 97, 99, 116, 0, 0, 0, 1, 9, 0, 4, 18, 0, 0, 0, 2, 8, 5, 0, 0, 0, 1, 100, 0,
      0, 0, 4, 100, 97, 116, 97, 2, 0, 0, 0, 4, 98, 108, 111, 98, 4, 0, 0, 0, 3, 115, 116, 114, 9, 1, 0, 0, 0, 7, 101, 120, 116, 114, 97, 99, 116, 0,
      0, 0, 1, 9, 0, 4, 19, 0, 0, 0, 2, 8, 5, 0, 0, 0, 1, 100, 0, 0, 0, 4, 100, 97, 116, 97, 2, 0, 0, 0, 3, 115, 116, 114, 4, 0, 0, 0, 9, 100, 97,
      116, 97, 66, 121, 75, 101, 121, 3, 3, 3, 9, 0, 0, 0, 0, 0, 0, 2, 9, 0, 1, -92, 0, 0, 0, 1, 5, 0, 0, 0, 3, 105, 110, 116, 2, 0, 0, 0, 2, 50, 52,
      6, 9, 0, 0, 0, 0, 0, 0, 2, 9, 0, 1, -91, 0, 0, 0, 1, 5, 0, 0, 0, 4, 98, 111, 111, 108, 2, 0, 0, 0, 4, 116, 114, 117, 101, 6, 9, 0, 0, 102, 0, 0,
      0, 2, 9, 0, 0, -56, 0, 0, 0, 1, 5, 0, 0, 0, 4, 98, 108, 111, 98, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 9, 0, 0, 0, 0, 0, 0, 2, 5, 0, 0, 0, 3, 115, 116,
      114, 2, 0, 0, 0, 4, 116, 101, 115, 116, 4, 0, 0, 0, 2, 100, 48, 9, 1, 0, 0, 0, 7, 101, 120, 116, 114, 97, 99, 116, 0, 0, 0, 1, 9, 1, 0, 0, 0,
      10, 103, 101, 116, 73, 110, 116, 101, 103, 101, 114, 0, 0, 0, 2, 8, 5, 0, 0, 0, 1, 100, 0, 0, 0, 4, 100, 97, 116, 97, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      4, 0, 0, 0, 2, 100, 49, 9, 1, 0, 0, 0, 7, 101, 120, 116, 114, 97, 99, 116, 0, 0, 0, 1, 9, 1, 0, 0, 0, 10, 103, 101, 116, 66, 111, 111, 108, 101,
      97, 110, 0, 0, 0, 2, 8, 5, 0, 0, 0, 1, 100, 0, 0, 0, 4, 100, 97, 116, 97, 0, 0, 0, 0, 0, 0, 0, 0, 1, 4, 0, 0, 0, 2, 100, 50, 9, 1, 0, 0, 0, 9,
      103, 101, 116, 66, 105, 110, 97, 114, 121, 0, 0, 0, 2, 8, 5, 0, 0, 0, 1, 100, 0, 0, 0, 4, 100, 97, 116, 97, 0, 0, 0, 0, 0, 0, 0, 0, 2, 4, 0, 0,
      0, 2, 100, 51, 9, 1, 0, 0, 0, 9, 103, 101, 116, 83, 116, 114, 105, 110, 103, 0, 0, 0, 2, 8, 5, 0, 0, 0, 1, 100, 0, 0, 0, 4, 100, 97, 116, 97, 0,
      0, 0, 0, 0, 0, 0, 0, 3, 4, 0, 0, 0, 11, 100, 97, 116, 97, 66, 121, 73, 110, 100, 101, 120, 3, 3, 3, 9, 0, 0, 0, 0, 0, 0, 2, 9, 0, 1, -102, 0, 0,
      0, 1, 5, 0, 0, 0, 2, 100, 48, 1, 0, 0, 0, 4, 105, -73, 29, 121, 6, 9, 0, 0, 0, 0, 0, 0, 2, 9, 0, 1, -100, 0, 0, 0, 1, 5, 0, 0, 0, 2, 100, 49, 1,
      0, 0, 0, 4, -126, 24, -93, -110, 6, 9, 1, 0, 0, 0, 9, 105, 115, 68, 101, 102, 105, 110, 101, 100, 0, 0, 0, 1, 5, 0, 0, 0, 2, 100, 50, 6, 9, 0,
      0, 0, 0, 0, 0, 2, 9, 0, 1, -101, 0, 0, 0, 1, 9, 1, 0, 0, 0, 7, 101, 120, 116, 114, 97, 99, 116, 0, 0, 0, 1, 5, 0, 0, 0, 2, 100, 51, 1, 0, 0, 0,
      4, -102, 122, 41, -86, 3, 5, 0, 0, 0, 9, 100, 97, 116, 97, 66, 121, 75, 101, 121, 5, 0, 0, 0, 11, 100, 97, 116, 97, 66, 121, 73, 110, 100, 101,
      120, 7, 3, 9, 0, 0, 1, 0, 0, 0, 2, 5, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104, 48, 2, 0, 0, 0, 19, 84, 114, 97, 110, 115, 102, 101, 114, 84, 114,
      97, 110, 115, 97, 99, 116, 105, 111, 110, 4, 0, 0, 0, 3, 97, 100, 100, 9, 1, 0, 0, 0, 7, 65, 100, 100, 114, 101, 115, 115, 0, 0, 0, 1, 1, 0, 0,
      0, 26, 1, 84, 100, 23, -115, -33, -128, -20, -97, 62, -33, -42, 86, -24, -22, 104, -110, -85, 40, -23, -25, 122, -18, -70, -100, -99, 4, 0, 0,
      0, 4, 108, 111, 110, 103, 9, 0, 0, 0, 0, 0, 0, 2, 9, 1, 0, 0, 0, 7, 101, 120, 116, 114, 97, 99, 116, 0, 0, 0, 1, 9, 0, 4, 26, 0, 0, 0, 2, 5, 0,
      0, 0, 3, 97, 100, 100, 2, 0, 0, 0, 3, 105, 110, 116, 0, 0, 0, 0, 0, 0, 0, 0, 24, 4, 0, 0, 0, 5, 98, 111, 111, 108, 49, 9, 0, 0, 0, 0, 0, 0, 2,
      9, 1, 0, 0, 0, 7, 101, 120, 116, 114, 97, 99, 116, 0, 0, 0, 1, 9, 0, 4, 27, 0, 0, 0, 2, 5, 0, 0, 0, 3, 97, 100, 100, 2, 0, 0, 0, 4, 98, 111,
      111, 108, 6, 4, 0, 0, 0, 3, 98, 105, 110, 9, 0, 0, 0, 0, 0, 0, 2, 9, 1, 0, 0, 0, 7, 101, 120, 116, 114, 97, 99, 116, 0, 0, 0, 1, 9, 0, 4, 28, 0,
      0, 0, 2, 5, 0, 0, 0, 3, 97, 100, 100, 2, 0, 0, 0, 4, 98, 108, 111, 98, 1, 0, 0, 0, 5, 97, 108, 105, 99, 101, 4, 0, 0, 0, 4, 115, 116, 114, 49,
      9, 0, 0, 0, 0, 0, 0, 2, 9, 1, 0, 0, 0, 7, 101, 120, 116, 114, 97, 99, 116, 0, 0, 0, 1, 9, 0, 4, 29, 0, 0, 0, 2, 5, 0, 0, 0, 3, 97, 100, 100, 2,
      0, 0, 0, 3, 115, 116, 114, 2, 0, 0, 0, 4, 116, 101, 115, 116, 3, 3, 3, 5, 0, 0, 0, 4, 108, 111, 110, 103, 5, 0, 0, 0, 5, 98, 111, 111, 108, 49,
      7, 5, 0, 0, 0, 3, 98, 105, 110, 7, 5, 0, 0, 0, 4, 115, 116, 114, 49, 7, 3, 9, 0, 0, 1, 0, 0, 0, 2, 5, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104, 48,
      2, 0, 0, 0, 22, 67, 114, 101, 97, 116, 101, 65, 108, 105, 97, 115, 84, 114, 97, 110, 115, 97, 99, 116, 105, 111, 110, 4, 0, 0, 0, 1, 97, 5, 0,
      0, 0, 7, 36, 109, 97, 116, 99, 104, 48, 9, 0, 0, 2, 0, 0, 0, 1, 2, 0, 0, 0, 5, 111, 104, 32, 110, 111, 3, 9, 0, 0, 1, 0, 0, 0, 2, 5, 0, 0, 0, 7,
      36, 109, 97, 116, 99, 104, 48, 2, 0, 0, 0, 15, 66, 117, 114, 110, 84, 114, 97, 110, 115, 97, 99, 116, 105, 111, 110, 4, 0, 0, 0, 1, 98, 5, 0, 0,
      0, 7, 36, 109, 97, 116, 99, 104, 48, 9, 1, 0, 0, 0, 5, 116, 104, 114, 111, 119, 0, 0, 0, 0, 7, 4, 0, 0, 0, 7, 97, 70, 114, 111, 109, 80, 75, 9,
      0, 0, 0, 0, 0, 0, 2, 9, 1, 0, 0, 0, 20, 97, 100, 100, 114, 101, 115, 115, 70, 114, 111, 109, 80, 117, 98, 108, 105, 99, 75, 101, 121, 0, 0, 0,
      1, 8, 5, 0, 0, 0, 2, 116, 120, 0, 0, 0, 15, 115, 101, 110, 100, 101, 114, 80, 117, 98, 108, 105, 99, 75, 101, 121, 8, 5, 0, 0, 0, 2, 116, 120,
      0, 0, 0, 6, 115, 101, 110, 100, 101, 114, 4, 0, 0, 0, 15, 97, 70, 114, 111, 109, 83, 116, 114, 79, 114, 82, 101, 99, 105, 112, 4, 0, 0, 0, 7,
      36, 109, 97, 116, 99, 104, 48, 5, 0, 0, 0, 2, 116, 120, 3, 9, 0, 0, 1, 0, 0, 0, 2, 5, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104, 48, 2, 0, 0, 0, 15,
      68, 97, 116, 97, 84, 114, 97, 110, 115, 97, 99, 116, 105, 111, 110, 9, 0, 0, 0, 0, 0, 0, 2, 9, 1, 0, 0, 0, 17, 97, 100, 100, 114, 101, 115, 115,
      70, 114, 111, 109, 83, 116, 114, 105, 110, 103, 0, 0, 0, 1, 2, 0, 0, 0, 35, 51, 78, 53, 71, 82, 113, 122, 68, 66, 104, 106, 86, 88, 110, 67,
      110, 52, 52, 98, 97, 72, 99, 122, 50, 71, 111, 90, 121, 53, 113, 76, 120, 116, 84, 104, 9, 1, 0, 0, 0, 7, 65, 100, 100, 114, 101, 115, 115, 0,
      0, 0, 1, 1, 0, 0, 0, 26, 1, 84, -88, 98, -11, -83, -97, -100, 82, 58, 6, 114, -79, -62, -117, -100, -95, 112, -63, 95, 104, -126, 95, -112, -18,
      0, 3, 9, 0, 0, 1, 0, 0, 0, 2, 5, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104, 48, 2, 0, 0, 0, 19, 84, 114, 97, 110, 115, 102, 101, 114, 84, 114, 97,
      110, 115, 97, 99, 116, 105, 111, 110, 4, 0, 0, 0, 2, 116, 49, 5, 0, 0, 0, 7, 36, 109, 97, 116, 99, 104, 48, 9, 0, 0, 0, 0, 0, 0, 2, 9, 0, 4, 36,
      0, 0, 0, 1, 8, 5, 0, 0, 0, 2, 116, 49, 0, 0, 0, 9, 114, 101, 99, 105, 112, 105, 101, 110, 116, 9, 1, 0, 0, 0, 7, 65, 100, 100, 114, 101, 115,
      115, 0, 0, 0, 1, 1, 0, 0, 0, 26, 1, 84, 100, 23, -115, -33, -128, -20, -97, 62, -33, -42, 86, -24, -22, 104, -110, -85, 40, -23, -25, 122, -18,
      -70, -100, -99, 7, 4, 0, 0, 0, 8, 98, 97, 108, 97, 110, 99, 101, 115, 3, 9, 0, 0, 102, 0, 0, 0, 2, 9, 0, 3, -21, 0, 0, 0, 2, 8, 5, 0, 0, 0, 2,
      116, 120, 0, 0, 0, 6, 115, 101, 110, 100, 101, 114, 5, 0, 0, 0, 4, 117, 110, 105, 116, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 1, 0, 0, 0, 2, 33, 61, 0,
      0, 0, 2, 9, 1, 0, 0, 0, 12, 119, 97, 118, 101, 115, 66, 97, 108, 97, 110, 99, 101, 0, 0, 0, 1, 8, 5, 0, 0, 0, 2, 116, 120, 0, 0, 0, 6, 115, 101,
      110, 100, 101, 114, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 4, 0, 0, 0, 5, 119, 97, 118, 101, 115, 3, 3, 3, 3, 3, 5, 0, 0, 0, 6, 116, 120, 66, 121, 73,
      100, 5, 0, 0, 0, 7, 101, 110, 116, 114, 105, 101, 115, 7, 5, 0, 0, 0, 8, 98, 97, 108, 97, 110, 99, 101, 115, 7, 5, 0, 0, 0, 7, 97, 70, 114, 111,
      109, 80, 75, 7, 5, 0, 0, 0, 15, 97, 70, 114, 111, 109, 83, 116, 114, 79, 114, 82, 101, 99, 105, 112, 7, 9, 0, 0, 102, 0, 0, 0, 2, 5, 0, 0, 0, 6,
      104, 101, 105, 103, 104, 116, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 4, 0, 0, 0, 3, 98, 107, 115, 3, 3, 9, 1, 0, 0, 0, 2, 33, 61, 0, 0, 0, 2, 9, 0, 1,
      -10, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 9, 1, 0, 0, 0, 2, 33, 61, 0, 0, 0, 2, 9, 0, 1, -11, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 7,
      9, 1, 0, 0, 0, 2, 33, 61, 0, 0, 0, 2, 9, 0, 1, -9, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 7, 4, 0, 0, 0, 3, 115, 105, 103, 9, 1, 0, 0, 0, 2,
      33, 61, 0, 0, 0, 2, 9, 0, 1, -12, 0, 0, 0, 3, 1, 0, 0, 0, 2, 26, -66, 1, 0, 0, 0, 2, 0, 60, 1, 0, 0, 0, 2, 53, -72, 6, 4, 0, 0, 0, 5, 115, 116,
      114, 53, 56, 9, 0, 0, 0, 0, 0, 0, 2, 9, 0, 2, 89, 0, 0, 0, 1, 9, 0, 2, 88, 0, 0, 0, 1, 8, 5, 0, 0, 0, 2, 116, 120, 0, 0, 0, 2, 105, 100, 8, 5,
      0, 0, 0, 2, 116, 120, 0, 0, 0, 2, 105, 100, 4, 0, 0, 0, 5, 115, 116, 114, 54, 52, 9, 0, 0, 0, 0, 0, 0, 2, 9, 0, 2, 91, 0, 0, 0, 1, 9, 0, 2, 90,
      0, 0, 0, 1, 8, 5, 0, 0, 0, 2, 116, 120, 0, 0, 0, 2, 105, 100, 8, 5, 0, 0, 0, 2, 116, 120, 0, 0, 0, 2, 105, 100, 4, 0, 0, 0, 6, 99, 114, 121,
      112, 116, 111, 3, 3, 3, 5, 0, 0, 0, 3, 98, 107, 115, 5, 0, 0, 0, 3, 115, 105, 103, 7, 5, 0, 0, 0, 5, 115, 116, 114, 53, 56, 7, 5, 0, 0, 0, 5,
      115, 116, 114, 54, 52, 7, 3, 5, 0, 0, 0, 3, 114, 110, 100, 3, 5, 0, 0, 0, 4, 112, 117, 114, 101, 5, 0, 0, 0, 5, 119, 97, 118, 101, 115, 7, 5, 0,
      0, 0, 6, 99, 114, 121, 112, 116, 111)
    Serde.serialize(compiledScript) shouldBe bytes
  }

}
