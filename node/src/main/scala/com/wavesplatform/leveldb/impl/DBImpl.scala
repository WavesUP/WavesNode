package com.wavesplatform.leveldb.impl

import java.nio.ByteBuffer
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicBoolean

import com.wavesplatform.leveldb._

class DBImpl(path: Path, options: Options) extends DB {
  private[this] val isClosed = new AtomicBoolean(false)
  private[this] val nativeOptions = new NativeOptions(options)
  private[this] val dbName = path.toAbsolutePath.toString
  private[this] val defaultReadOptions: NativeLevelDB.ReadOptions = NativeLevelDB.leveldb_readoptions_create()
  private[this] val defaultWriteOptions: NativeLevelDB.WriteOptions = NativeLevelDB.leveldb_writeoptions_create()
  private[this] val nativeDB = throwingError { ptr =>
    NativeLevelDB.leveldb_open(nativeOptions.ptr, dbName, ptr)
  }

  override def get[A](key: Array[Byte])(f: ByteBuffer => A): Option[A] = impl.getValue(nativeDB, defaultReadOptions, key)(f)

  override def get[A](key: Array[Byte], readOptions: ReadOptions)(f: ByteBuffer => A): Option[A] = impl.getValue(nativeDB, readOptions, key)(f)

  override def put(key: Array[Byte], value: Array[Byte]): Unit = impl.put(nativeDB, defaultWriteOptions, key, value)

  override def delete(key: Array[Byte]): Unit = impl.delete(nativeDB, defaultWriteOptions, key)

  override def keyIterator() = new KeyIteratorImpl(nativeDB, defaultReadOptions)

  override def keyValueIterator() = ???

  override def view(): View = ???

  override def repair(): Unit = throwingError { ptr =>
    NativeLevelDB.leveldb_repair_db(nativeOptions.ptr, dbName, ptr)
  }

  override def destroy(): Unit = throwingError { ptr =>
    NativeLevelDB.leveldb_destroy_db(nativeOptions.ptr, dbName, ptr)
  }

  override def version: (Int, Int) =
    NativeLevelDB.leveldb_major_version() -> NativeLevelDB.leveldb_minor_version()

  override def property(key: Property.Key): String =
    NativeLevelDB.leveldb_property_value(nativeDB, key.name)

  override def close(): Unit = if (isClosed.compareAndSet(false, true)) {
    NativeLevelDB.leveldb_readoptions_destroy(defaultReadOptions)
    NativeLevelDB.leveldb_writeoptions_destroy(defaultWriteOptions)
    nativeOptions.close()
    NativeLevelDB.leveldb_close(nativeDB)
  }

  override def snapshot(): Snapshot = new SnapshotImpl(nativeDB)
}
