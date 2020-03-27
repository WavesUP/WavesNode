package com.wavesplatform.leveldb

import java.nio.ByteBuffer

import com.sun.jna.ptr.PointerByReference
import com.wavesplatform.leveldb.impl.{NativeLevelDB => N}

package object impl {
  val TRUE = 1.toByte
  val FALSE = 0.toByte

  implicit class BooleanExt(val flag: Boolean) extends AnyVal {
    def toByte: Byte = if (flag) TRUE else FALSE
  }

  implicit class LongExt(val v: Long) extends AnyVal {
    def toSizeT: size_t = new size_t(v)
  }

  def throwingError[A](f: PointerByReference => A): A = {
    val ptr = new PointerByReference
    val result = f(ptr)

    Option(ptr.getValue).foreach { e =>
      throw new LevelDBException(e.getString(0))
    }

    result
  }

  private[impl] def getValue[A](levelDB: N.LevelDB, readOptions: ReadOptions, key: Array[Byte])(f: ByteBuffer => A): Option[A] = {
    val nativeReadOptions = new NativeReadOptions(readOptions)
    try impl.getValue(levelDB, nativeReadOptions.ptr, key)(f)
    finally nativeReadOptions.close()
  }

  private[impl] def getValue[A](levelDB: N.LevelDB, readOptions: N.ReadOptions, key: Array[Byte])(f: ByteBuffer => A): Option[A] = {
    val resultLengthPointer = new PointerByReference()
    val keyLength = new size_t(if (key != null) key.length else 0)
    val result = throwingError { error =>
      N.leveldb_get(levelDB, readOptions, key, keyLength, resultLengthPointer, error)
    }

    val resultLength = resultLengthPointer.getPointer.getNativeLong(0)

    Option(result).map { r =>
      val resultValue = f(r.getPointer.getByteBuffer(0, resultLength.longValue()).asReadOnlyBuffer())
      N.leveldb_free(result.getPointer)
      resultValue
    }
  }

  private[impl] def get(levelDB: N.LevelDB, readOptions: NativeLevelDB.ReadOptions, key: Array[Byte]): Option[Array[Byte]] = {
    val resultLengthPointer = new PointerByReference()
    val keyLength = new size_t(if (key != null) key.length else 0)
    val result = throwingError { error =>
      N.leveldb_get(levelDB, readOptions, key, keyLength, resultLengthPointer, error)
    }

    val resultLength = resultLengthPointer.getPointer.getNativeLong(0)

    Option(result).map { r =>
      val resultAsByteArray = r.getPointer.getByteArray(0, resultLength.intValue())
      N.leveldb_free(result.getPointer)
      resultAsByteArray
    }
  }

  private[impl] def put(levelDB: N.LevelDB, writeOptions: NativeLevelDB.WriteOptions, key: Array[Byte], value: Array[Byte]): Unit = throwingError { errPtr =>
    val keyLength = new size_t(if (key != null) key.length else 0)
    val valueLength = new size_t(if (value != null) value.length else 0)
    N.leveldb_put(levelDB, writeOptions, key, keyLength, value, valueLength, errPtr)
  }

  private[impl] def delete(levelDB: N.LevelDB, writeOptions: NativeLevelDB.WriteOptions, key: Array[Byte]): Unit = throwingError { errPtr =>
    val keyLength = new size_t(if (key != null) key.length else 0)
    N.leveldb_delete(levelDB, writeOptions, key, keyLength, errPtr)
  }
}
