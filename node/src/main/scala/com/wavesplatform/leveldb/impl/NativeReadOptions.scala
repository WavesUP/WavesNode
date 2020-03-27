package com.wavesplatform.leveldb.impl

import com.wavesplatform.leveldb.ReadOptions
import com.wavesplatform.leveldb.impl.NativeLevelDB._

class NativeReadOptions(val readOptions: ReadOptions) extends AutoCloseable {
  private[impl] val ptr = leveldb_readoptions_create()

  leveldb_readoptions_set_fill_cache(ptr, readOptions.fillCache.toByte)
  leveldb_readoptions_set_verify_checksums(ptr, readOptions.verifyChecksums.toByte)
  readOptions.snapshot match {
    case Some(ns: SnapshotImpl) => leveldb_readoptions_set_snapshot(ptr, ns.nativeSnapshot)
    case _ =>
  }

  override def close(): Unit = {
    leveldb_readoptions_destroy(ptr)
  }
}
