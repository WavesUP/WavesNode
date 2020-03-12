package com.wavesplatform.it.account.storage

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.BaseSuite
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_BYTESTR, CONST_LONG, CONST_STRING}
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class RemoveEntrySuite extends BaseSuite {

  def writeEntry(t: String, v: String) =
    s"""
       |@Callable(i) func write$t(k: String, v: ${if (t.contains("Int")) "Int" else t}) = [${t}Entry(k, v${if (t.equals("Binary")) ".toBytes" else ""})]
       """.stripMargin

  val writeEntriesFunc = s"@Callable(i) func write%sEntries() = FOLD<%s>(a, [], writeEntry)"

  def deleteEntriesFunc(c: Int) = s"@Callable(i) func delete${c}Entries() = FOLD<$c>(a, [], deleteEntry)"

  val script = """
                 |{-# STDLIB_VERSION 4 #-}
                 |{-# SCRIPT_TYPE ACCOUNT #-}
                 |{-# CONTENT_TYPE DAPP #-}
                 |
                 |let a = [
                 |  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
                 |  21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
                 |  41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
                 |  61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80,
                 |  81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99
                 |]
                 |
                 |func writeEntry(acc: List[StringEntry], e: Int) = StringEntry(e.toString(), "value") :: acc
                 |
                 |func deleteEntry(acc: List[DeleteEntry], e: Int) = DeleteEntry(e.toString()) :: acc
                 |
                 |@Callable(i) func write(k: String, v: String) = [StringEntry(k, v)]
                 |
                 |@Callable(i) func delete(k: String) = [DeleteEntry(k)]
                 |
       """.stripMargin

  "Remove entry from account storage" - {

    "Simple remove one entry" in {
      val address = createDapp(script)

      val txWrite = miner
        .invokeScript(
          address,
          address,
          fee = smartMinFee + smartFee,
          waitForTx = true,
          func = Some("write"),
          args = List(CONST_STRING("someKey1").explicitGet(), CONST_STRING("someValue1").explicitGet())
        )
        ._1
        .id
      nodes.waitForHeightAriseAndTxPresent(txWrite)

      miner.getData(address) should have size 1

      val txDelete = miner
        .invokeScript(
          address,
          address,
          fee = smartMinFee + smartFee,
          waitForTx = true,
          func = Some("delete"),
          args = List(CONST_STRING("someKey1").explicitGet())
        )
        ._1
        .id
      nodes.waitForHeightAriseAndTxPresent(txDelete)

      miner.getData(address) should have size 0
    }
  }

  def createDapp(scriptParts: String*): String = {
    val script  = scriptParts.mkString(" ")
    val address = miner.createAddress()
    val compiledScript = ScriptCompiler
      .compile(
        script,
        ScriptEstimatorV2
      )
      .explicitGet()
      ._1

    miner.transfer(sender.address, address, 10.waves, minFee, waitForTx = true)

    nodes.waitForHeightAriseAndTxPresent(
      miner
        .signedBroadcast(
          SetScriptTransaction
            .selfSigned(1.toByte, KeyPair(Base58.decode(miner.seed(address))), Some(compiledScript), setScriptFee, System.currentTimeMillis())
            .explicitGet()
            .json
            .value
        )
        .id
    )

    address
  }

  def invokeScript(address: String, function: String, key: String = "", value: String = "", wait: Boolean = true): String = {
    val args = function match {
      case "write"            => List(CONST_STRING(key).explicitGet(), CONST_STRING(value).explicitGet())
      case "writeString"      => List(CONST_STRING(key).explicitGet(), CONST_STRING(value).explicitGet())
      case "writeInteger"     => List(CONST_STRING(key).explicitGet(), CONST_LONG(value.toLong))
      case "writeBoolean"     => List(CONST_STRING(key).explicitGet(), CONST_BOOLEAN(value.toBoolean))
      case "writeBinary"      => List(CONST_STRING(key).explicitGet(), CONST_BYTESTR(value.getBytes()).explicitGet)
      case "delete"           => List(CONST_STRING(key).explicitGet())
      case "delete100Entries" => List.empty
      case "delete101Entries" => List.empty
      case _                  => List.empty
    }


    val tx = miner
      .invokeScript(
        address,
        address,
        fee = smartMinFee + smartFee,
        waitForTx = wait,
        func = Some(function),
        args = args
      )
      ._1
      .id

    if (wait) nodes.waitForHeightAriseAndTxPresent(tx)
    tx
  }
}
