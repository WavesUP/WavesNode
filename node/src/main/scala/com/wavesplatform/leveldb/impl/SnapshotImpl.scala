package com.wavesplatform.leveldb.impl

import com.wavesplatform.leveldb.Snapshot

class SnapshotImpl(nativeDB: NativeLevelDB.LevelDB) extends Snapshot with AutoCloseable {
  private[impl] val nativeSnapshot = NativeLevelDB.leveldb_create_snapshot(nativeDB)

  override def close(): Unit = NativeLevelDB.leveldb_release_snapshot(nativeDB, nativeSnapshot)
}
