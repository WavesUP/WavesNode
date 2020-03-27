package com.wavesplatform.leveldb.impl

import com.wavesplatform.leveldb.WriteOptions

class NativeWriteOptions(writeOptions: WriteOptions) extends AutoCloseable {
  private[impl] val ptr = NativeLevelDB.leveldb_writeoptions_create()

  NativeLevelDB.leveldb_writeoptions_set_sync(ptr, writeOptions.sync.toByte)

  override def close(): Unit = NativeLevelDB.leveldb_writeoptions_destroy(ptr)
}
