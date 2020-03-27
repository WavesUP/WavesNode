package com.wavesplatform.leveldb

import java.nio.ByteBuffer
import java.nio.file.Path

import com.google.common.base.CaseFormat
import com.wavesplatform.leveldb.impl.{ByteBufferOps, DBImpl}

trait Snapshot extends AutoCloseable

case class ReadOptions(
    verifyChecksums: Boolean = false,
    fillCache: Boolean = true,
    snapshot: Option[Snapshot] = None
)

case class WriteOptions(sync: Boolean = false)

case class BloomFilter(bitsPerKey: Int)

case class Options(
    createIfMissing: Boolean = false,
    errorIfExists: Boolean = false,
    paranoidChecks: Boolean = false,
    writeBufferSize: Long = 4 * 1024 * 1024,
    maxOpenFiles: Int = 1000,
    blockCacheSize: Option[Long] = None,
    blockSize: Long = 4 * 1024,
    blockRestartInterval: Int = 16,
    maxFileSize: Long = 2 * 1024 * 1024,
    snappyCompression: Boolean = true,
    filterPolicy: Option[BloomFilter] = None
)

trait SeekingIterator[A] extends Iterator[A] with AutoCloseable {
  def seek(toKey: Array[Byte]): Unit

  def seekToFirst(): Unit

  def seekToLast(): Unit

  def prev(): A

  def hasPrev: Boolean

  def iterateOver(prefix: Array[Byte])(f: A => Unit): Unit
}

trait KeyIterator extends SeekingIterator[Array[Byte]]

trait KeyValueIterator extends SeekingIterator[(Array[Byte], Array[Byte])]

trait View extends AutoCloseable {
  def keyIterator: KeyIterator

  def keyValueIterator: KeyValueIterator

  def get(key: Array[Byte]): Option[Array[Byte]]

  def put(key: Array[Byte], value: Array[Byte])

  def delete(key: Array[Byte]): Unit
}

sealed trait Property {
  def key: String = Property.converter(getClass.getSimpleName)
}

object Property extends Enumeration {
  case class NumFilesAtLevel(level: Int) extends Property {
    override val key = super.key + level.toString
  }
  case object Stats                  extends Property
  case object Sstables               extends Property
  case object ApproximateMemoryUsage extends Property

  private val converter = CaseFormat.UPPER_CAMEL.converterTo(CaseFormat.LOWER_HYPHEN).andThen("leveldb." + _)
}

trait DB extends AutoCloseable {
  def get[A](key: Array[Byte])(f: ByteBuffer => A): Option[A]

  def get[A](key: Array[Byte], readOptions: ReadOptions = ReadOptions())(f: ByteBuffer => A): Option[A]

  def put(key: Array[Byte], value: Array[Byte]): Unit

  def delete(key: Array[Byte]): Unit

  def keyIterator(): SeekingIterator[Array[Byte]]

  def keyValueIterator(): SeekingIterator[(Array[Byte], Array[Byte])]

  def view(): View

  def repair(): Unit

  def destroy(): Unit

  def property(property: Property): String

  def version: (Int, Int)

  def snapshot(): Snapshot
}

object DB {

  implicit class DBExt(val db: DB) extends AnyVal {
    def getString(key: Array[Byte]): Option[String] = db.get(key)(ByteBufferOps.getUtf8String)

    def getString(key: Array[Byte], readOptions: ReadOptions): Option[String] = db.get(key, readOptions)(ByteBufferOps.getUtf8String)
  }

  def apply(path: Path, options: Options = Options()): DB = new DBImpl(path, options)
}
