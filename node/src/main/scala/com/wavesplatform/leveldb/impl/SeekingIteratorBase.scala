package com.wavesplatform.leveldb.impl

import java.nio.ByteBuffer

import com.sun.jna.ptr.PointerByReference
import com.wavesplatform.leveldb.{ReadOptions, SeekingIterator, impl}

import scala.annotation.tailrec

abstract class SeekingIteratorBase[A](nativeDB: NativeLevelDB.LevelDB, nativeReadOptions: NativeReadOptions) extends SeekingIterator[A] {
  private[this] var readOptions = Option.empty[NativeLevelDB.ReadOptions]
  private[this] val nativeIterator = NativeLevelDB.leveldb_create_iterator(nativeDB, nativeReadOptions.ptr)

  def this(nativeDB: NativeLevelDB.LevelDB, readOptions: ReadOptions) = this(nativeDB, new NativeReadOptions(readOptions))

  def this(nativeDB: NativeLevelDB.LevelDB) = {
    this(nativeDB, new NativeReadOptions(ReadOptions()))
    readOptions = Some(nativeReadOptions.ptr)
  }

  private[this] def throwingError[R](f: => R): R = impl.throwingError { errPtr =>
    val result = f
    NativeLevelDB.leveldb_iter_get_error(nativeIterator, errPtr)
    result
  }

  protected def currentValue(): A

  protected def currentKey: PointerByReference = {
    val p = new PointerByReference()
    NativeLevelDB.leveldb_iter_key(nativeIterator, p)
    p
  }

  protected def currentBytes[R](f: => PointerByReference)(decoder: ByteBuffer => R): R = {
    val resultPointer = f
    val length = resultPointer.getPointer.getNativeLong(0)
    val decodedResult = decoder(resultPointer.getPointer.getByteBuffer(0, length.longValue()))
    NativeLevelDB.leveldb_free(resultPointer.getPointer)
    decodedResult
  }

  private def isValid: Boolean = throwingError {
    NativeLevelDB.leveldb_iter_valid(nativeIterator) != impl.FALSE
  }

  override def next(): A = throwingError {
    val cv = currentValue()
    NativeLevelDB.leveldb_iter_next(nativeIterator)
    cv
  }

  override def prev(): A = throwingError {
    val cv = currentValue()
    NativeLevelDB.leveldb_iter_prev(nativeIterator)
    cv
  }

  override def seekToFirst(): Unit = throwingError {
    NativeLevelDB.leveldb_iter_seek_to_first(nativeIterator)
  }

  override def seekToLast(): Unit = throwingError {
    NativeLevelDB.leveldb_iter_seek_to_last(nativeIterator)
  }

  override def seek(toKey: Array[Byte]): Unit = throwingError {
    val keyLength = new size_t(if (toKey != null) toKey.length else 0)
    NativeLevelDB.leveldb_iter_seek(nativeIterator, toKey, keyLength)
  }

  override def hasNext: Boolean = isValid

  override def hasPrev: Boolean = isValid

  override def close(): Unit = {
    NativeLevelDB.leveldb_iter_destroy(nativeIterator)
    readOptions.foreach(NativeLevelDB.leveldb_readoptions_destroy)
  }


  @tailrec
  private final def iterationStep(prefix: Array[Byte])(f: A => Unit): Unit =
    if (isValid && currentBytes(currentKey) { b => prefix.forall(_ == b.get()) }) {
      f(next())
      iterationStep(prefix)(f)
    }

  override def iterateOver(prefix: Array[Byte])(f: A => Unit): Unit = {
    seek(prefix)
    iterationStep(prefix)(f)
  }
}
