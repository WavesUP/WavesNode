package com.wavesplatform.lang.v1.evaluator.ctx.impl

import cats.implicits._
import cats.{Id, Monad}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, _}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.compiler.{CompilerContext, Terms}
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.ContextfulVal
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA.DigestAlgorithm
import com.wavesplatform.lang.v1.evaluator.ctx.{BaseFunction, EvaluationContext, NativeFunction}
import com.wavesplatform.lang.v1.{BaseGlobal, CTX}

import scala.util.Try

object CryptoContext {

  private val rsaTypeNames = List("NoAlg", "Md5", "Sha1", "Sha224", "Sha256", "Sha384", "Sha512", "Sha3224", "Sha3256", "Sha3384", "Sha3512")

  private def rsaHashAlgs(v: StdLibVersion) = {
    rsaTypeNames.map(CASETYPEREF(_, List.empty))
  }

  private def digestAlgorithmType(v: StdLibVersion) =
    UNION.create(rsaHashAlgs(v), (if(v > V3) { Some("RsaDigestAlgs") } else { None }))

  private val rsaHashLib = {
    import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA._
    rsaTypeNames.zip(List(NONE, MD5, SHA1, SHA224, SHA256, SHA384, SHA512, SHA3224, SHA3256, SHA3384, SHA3512)).toMap
  }

  private def algFromCO(obj: Terms.CaseObj): Either[String, DigestAlgorithm] = {
    rsaHashLib.get(obj.caseType.name).fold(Left[String, DigestAlgorithm]("Unknown digest type"): Either[String, DigestAlgorithm])(Right(_))
  }

  private def digestAlgValue(tpe: CASETYPEREF): ContextfulVal[NoContext] =
    ContextfulVal.pure(CaseObj(tpe, Map.empty))

  def build(global: BaseGlobal, version: StdLibVersion): CTX[NoContext] = {
    def lgen(
        lim: Array[Int],
        name: ((Int, Int)) => (String, Short),
        complexity: Int => Int,
        check: Int => List[EVALUATED] => Either[ExecutionError, Unit],
        ret: TYPE,
        args: (String, TYPE)*
    )(body: List[EVALUATED] => Either[ExecutionError, EVALUATED]): Array[BaseFunction[NoContext]] = {
      lim.zipWithIndex.map { n =>
        val (sname, iname) = name(n)
        NativeFunction[NoContext](sname, complexity(n._1), iname, ret, args: _*) { a =>
          check(n._1)(a).flatMap(_ => body(a))
        }
      }
    }

    def hashFunction(name: String, internalName: Short, cost: Long)(h: Array[Byte] => Array[Byte]): BaseFunction[NoContext] =
      NativeFunction(name, cost, internalName, BYTESTR, ("bytes", BYTESTR)) {
        case CONST_BYTESTR(m: ByteStr) :: Nil => CONST_BYTESTR(ByteStr(h(m.arr)))
        case xs                               => notImplemented[Id](s"$name(bytes: ByteVector)", xs)
      }

    val keccak256F: BaseFunction[NoContext] = hashFunction("keccak256", KECCAK256, (if (version < V4) {
                                                                                      10
                                                                                    } else {
                                                                                      200
                                                                                    }))(global.keccak256)
    val blake2b256F: BaseFunction[NoContext] = hashFunction("blake2b256", BLAKE256, (if (version < V4) {
                                                                                       10
                                                                                     } else {
                                                                                       200
                                                                                     }))(global.blake2b256)
    val sha256F: BaseFunction[NoContext] = hashFunction("sha256", SHA256, (if (version < V4) {
                                                                             10
                                                                           } else {
                                                                             200
                                                                           }))(global.sha256)

    def sigVerifyF(contextVer: StdLibVersion): BaseFunction[NoContext] = {
      val lim = global.MaxByteStrSizeForVerifyFuncs
      NativeFunction("sigVerify", (if (version < V4) {
                                     100
                                   } else {
                                     200
                                   }), SIGVERIFY, BOOLEAN, ("message", BYTESTR), ("sig", BYTESTR), ("pub", BYTESTR)) {
        case CONST_BYTESTR(msg: ByteStr) :: CONST_BYTESTR(sig: ByteStr) :: CONST_BYTESTR(pub: ByteStr) :: Nil
            if (contextVer == V3 && msg.size > lim) =>
          Left(s"Invalid message size = ${msg.size} bytes, must be not greater than ${lim / 1024} KB")
        case CONST_BYTESTR(msg: ByteStr) :: CONST_BYTESTR(sig: ByteStr) :: CONST_BYTESTR(pub: ByteStr) :: Nil =>
          Right(CONST_BOOLEAN(global.curve25519verify(msg.arr, sig.arr, pub.arr)))
        case xs => notImplemented[Id](s"sigVerify(message: ByteVector, sig: ByteVector, pub: ByteVector)", xs)
      }
    }

    val rsaVerifyF: BaseFunction[NoContext] = {
      val lim = global.MaxByteStrSizeForVerifyFuncs
      NativeFunction(
        "rsaVerify",
        (if (version < V4) {
           300
         } else {
           1000
         }),
        RSAVERIFY,
        BOOLEAN,
        ("digest", digestAlgorithmType(version)),
        ("message", BYTESTR),
        ("sig", BYTESTR),
        ("pub", BYTESTR)
      ) {
        case (digestAlg: CaseObj) :: CONST_BYTESTR(msg: ByteStr) :: CONST_BYTESTR(sig: ByteStr) :: CONST_BYTESTR(pub: ByteStr) :: Nil
            if version < V4 && msg.size > lim =>
          Left(s"Invalid message size = ${msg.size} bytes, must be not greater than ${lim / 1024} KB")
        case (digestAlg: CaseObj) :: CONST_BYTESTR(msg: ByteStr) :: CONST_BYTESTR(sig: ByteStr) :: CONST_BYTESTR(pub: ByteStr) :: Nil =>
          algFromCO(digestAlg) flatMap { alg =>
            Try(global.rsaVerify(alg, msg.arr, sig.arr, pub.arr)).toEither
              .bimap(_ => "Illegal input params", CONST_BOOLEAN)
          }
        case xs => notImplemented[Id](s"rsaVerify(digest: DigestAlgorithmType, message: ByteVector, sig: ByteVector, pub: ByteVector)", xs)
      }
    }

    val rsaVerifyL: Array[BaseFunction[NoContext]] = lgen(
      Array(16, 32, 64, 128),
      (n => (s"rsaVerify_${n._1}Kb", (RSAVERIFY_LIM + n._2).toShort)),
      ({
        case 16  => 500
        case 32  => 550
        case 64  => 625
        case 128 => 750
      }),
      (n => {
        case _ :: CONST_BYTESTR(msg: ByteStr) :: _ => Either.cond(msg.size <= n * 1024, (), s"Invalid message size = ${msg.size} bytes, must be not greater than $n KB")
        case xs =>
          notImplemented[Id, Unit](s"rsaVerify_${n}Kb(digest: DigestAlgorithmType, message: ByteVector, sig: ByteVector, pub: ByteVector)", xs)
      }),
      BOOLEAN,
      ("digest", digestAlgorithmType(V4)),
      ("message", BYTESTR),
      ("sig", BYTESTR),
      ("pub", BYTESTR)
    ) {
      case (digestAlg: CaseObj) :: CONST_BYTESTR(msg: ByteStr) :: CONST_BYTESTR(sig: ByteStr) :: CONST_BYTESTR(pub: ByteStr) :: Nil =>
        algFromCO(digestAlg) flatMap { alg =>
          Try(global.rsaVerify(alg, msg.arr, sig.arr, pub.arr)).toEither
            .bimap(_ => "Illegal input params", CONST_BOOLEAN)
        }
      case xs => notImplemented[Id, EVALUATED](s"rsaVerify(digest: DigestAlgorithmType, message: ByteVector, sig: ByteVector, pub: ByteVector)", xs)
    }

    def toBase58StringF: BaseFunction[NoContext] =
      NativeFunction(
        "toBase58String",
        Map[StdLibVersion, Long](V1 -> 10L, V2 -> 10L, V3 -> 10L, V4 -> 3L),
        TOBASE58,
        STRING,
        ("bytes", BYTESTR)
      ) {
      case CONST_BYTESTR(bytes: ByteStr) :: Nil => global.base58Encode(bytes.arr).flatMap(CONST_STRING(_, reduceLimit = version >= V4))
      case xs                                   => notImplemented[Id, EVALUATED]("toBase58String(bytes: ByteVector)", xs)
    }

    def fromBase58StringF: BaseFunction[NoContext] =
      NativeFunction(
        "fromBase58String",
        Map[StdLibVersion, Long](V1 -> 10L, V2 -> 10L, V3 -> 10L, V4 -> 1L),
        FROMBASE58,
        BYTESTR,
        ("str", STRING)
      ) {
        case CONST_STRING(str: String) :: Nil => global.base58Decode(str, global.MaxBase58String).flatMap(x => CONST_BYTESTR(ByteStr(x)))
        case xs                               => notImplemented[Id]("fromBase58String(str: String)", xs)
      }

    def toBase64StringF: BaseFunction[NoContext] =
      NativeFunction(
        "toBase64String",
        Map[StdLibVersion, Long](V1 -> 10L, V2 -> 10L, V3 -> 10L, V4 -> 35L),
        TOBASE64,
        STRING,
        ("bytes", BYTESTR)
      ) {
      case CONST_BYTESTR(bytes: ByteStr) :: Nil => global.base64Encode(bytes.arr).flatMap(CONST_STRING(_, reduceLimit = version >= V4))
      case xs                                   => notImplemented[Id, EVALUATED]("toBase64String(bytes: ByteVector)", xs)
    }

    def fromBase64StringF: BaseFunction[NoContext] =
      NativeFunction(
        "fromBase64String",
        Map[StdLibVersion, Long](V1 -> 10L, V2 -> 10L, V3 -> 10L, V4 -> 40L),
        FROMBASE64,
        BYTESTR,
        ("str", STRING)
      ) {
        case CONST_STRING(str: String) :: Nil => global.base64Decode(str, global.MaxBase64String).flatMap(x => CONST_BYTESTR(ByteStr(x)))
        case xs                               => notImplemented[Id]("fromBase64String(str: String)", xs)
      }

    val checkMerkleProofF: BaseFunction[NoContext] =
      NativeFunction(
        "checkMerkleProof",
        30,
        CHECK_MERKLE_PROOF,
        BOOLEAN,
        ("merkleRoot", BYTESTR),
        ("merkleProof", BYTESTR),
        ("valueBytes", BYTESTR)
      ) {
        case CONST_BYTESTR(root) :: CONST_BYTESTR(proof) :: CONST_BYTESTR(value) :: Nil =>
          Right(CONST_BOOLEAN(global.merkleVerify(root.arr, proof.arr, value.arr)))
        case xs => notImplemented[Id](s"checkMerkleProof(merkleRoot: ByteVector, merkleProof: ByteVector, valueBytes: ByteVector)", xs)
      }

    def toBase16StringF(checkLength: Boolean): BaseFunction[NoContext] = NativeFunction("toBase16String", 10, TOBASE16, STRING, ("bytes", BYTESTR)) {
      case CONST_BYTESTR(bytes: ByteStr) :: Nil => global.base16Encode(bytes.arr).flatMap(CONST_STRING(_))
      case xs                                   => notImplemented[Id]("toBase16String(bytes: ByteVector)", xs)
    }

    def fromBase16StringF(checkLength: Boolean): BaseFunction[NoContext] =
      NativeFunction("fromBase16String", 10, FROMBASE16, BYTESTR, ("str", STRING)) {
        case CONST_STRING(str: String) :: Nil => global.base16Decode(str).flatMap(x => CONST_BYTESTR(ByteStr(x)))
        case xs                               => notImplemented[Id]("fromBase16String(str: String)", xs)
      }

    val v1Functions =
      Array(
        keccak256F,
        blake2b256F,
        sha256F,
        sigVerifyF(version),
        toBase58StringF,
        fromBase58StringF,
        toBase64StringF,
        fromBase64StringF
      )

    val rsaVarNames = List("NOALG", "MD5", "SHA1", "SHA224", "SHA256", "SHA384", "SHA512", "SHA3224", "SHA3256", "SHA3384", "SHA3512")

    val v4RsaDig = rsaHashAlgs(V4)
    val v4Types = v4RsaDig :+ digestAlgorithmType(V4)

    val v4Vars: Map[String, (FINAL, ContextfulVal[NoContext])] =
      rsaVarNames.zip(v4RsaDig.map(t => (t, digestAlgValue(t)))).toMap

    val v3RsaDig = rsaHashAlgs(V3)
    val v3Types = v3RsaDig :+ digestAlgorithmType(V3)

    val v3Vars: Map[String, (FINAL, ContextfulVal[NoContext])] =
      rsaVarNames.zip(v3RsaDig.map(t => (t, digestAlgValue(t)))).toMap

    val v3Functions =
      Array(
        rsaVerifyF,
        checkMerkleProofF,
        toBase16StringF(checkLength = false),
        fromBase16StringF(checkLength = false)
      )

    val v4Functions =
      Array(
        rsaVerifyF,
        toBase16StringF(checkLength = true),
        fromBase16StringF(checkLength = true) // from V3
      )

    val fromV1Ctx = CTX[NoContext](Seq(), Map(), v1Functions)
    val fromV3Ctx = fromV1Ctx |+| CTX[NoContext](v3Types, v3Vars, v3Functions)
    val fromV4Ctx = fromV1Ctx |+| CTX[NoContext](v4Types, v4Vars, v4Functions)

    version match {
      case V1 | V2      => fromV1Ctx
      case V3           => fromV3Ctx
      case v if v >= V4 => fromV4Ctx
    }
  }

  def evalContext[F[_]: Monad](global: BaseGlobal, version: StdLibVersion): EvaluationContext[NoContext, F] =
    build(global, version).evaluationContext[F]

  def compilerContext(global: BaseGlobal, version: StdLibVersion): CompilerContext =
    build(global, version).compilerContext
}
