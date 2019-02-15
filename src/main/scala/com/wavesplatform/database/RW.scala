package com.wavesplatform.database

import com.wavesplatform.common.utils.Base64
import com.wavesplatform.metrics.LevelDBStats
import com.wavesplatform.metrics.LevelDBStats.DbHistogramExt
import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb.{DB, ReadOptions, WriteBatch}

class RW(db: DB, readOptions: ReadOptions, batch: WriteBatch) extends ReadOnlyDB(db, readOptions) with ScorexLogging {
  def put[V](key: Key[V], value: V): Unit = {
    log.info(s"PUT: ${key.name} ${Base64.encode(key.keyBytes)} - $value")
    val bytes = key.encode(value)
    LevelDBStats.write.recordTagged(key, bytes)
    batch.put(key.keyBytes, bytes)
  }

  def update[V](key: Key[V])(f: V => V): Unit = put(key, f(get(key)))

  /** Because of how leveldb batches work, you can increment a specific value only once! */
  def inc(key: Key[Int]): Int = {
    val newValue = get(key) + 1
    put(key, newValue)
    newValue
  }

  def delete(key: Array[Byte], statsKey: String): Unit = {
    log.info(s"DELETE: [$statsKey] - ${Base64.encode(key)}")
    batch.delete(key)
  }

  def delete[V](key: Key[V]): Unit = {
    log.info(s"DELETE: ${key.name} - ${Base64.encode(key.keyBytes)}")
    batch.delete(key.keyBytes)
  }

  def filterHistory(key: Key[Seq[Int]], heightToRemove: Int): Unit = put(key, get(key).filterNot(_ == heightToRemove))
}
