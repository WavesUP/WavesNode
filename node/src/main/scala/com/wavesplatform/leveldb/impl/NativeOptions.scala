package com.wavesplatform.leveldb.impl

import com.wavesplatform.leveldb.Options
import com.wavesplatform.leveldb.impl.NativeLevelDB._

class NativeOptions(val options: Options) extends AutoCloseable {

  val ptr = leveldb_options_create()

  private val nativeFilterPolicy = options.filterPolicy.map { bf =>
    leveldb_filterpolicy_create_bloom(bf.bitsPerKey)
  }

  private val nativeCache = options.blockCacheSize.map { cs =>
    leveldb_cache_create_lru(cs.toSizeT)
  }

  leveldb_options_set_block_restart_interval(ptr, options.blockRestartInterval)
  leveldb_options_set_create_if_missing(ptr, options.createIfMissing.toByte)
  leveldb_options_set_error_if_exists(ptr, options.errorIfExists.toByte)
  leveldb_options_set_paranoid_checks(ptr, options.paranoidChecks.toByte)
  leveldb_options_set_max_open_files(ptr, options.maxOpenFiles)
  leveldb_options_set_block_restart_interval(ptr, options.blockRestartInterval)
  leveldb_options_set_compression(ptr, options.snappyCompression.toByte)

  leveldb_options_set_write_buffer_size(ptr, options.writeBufferSize.toSizeT)
  leveldb_options_set_block_size(ptr, options.blockSize.toSizeT)
  leveldb_options_set_max_file_size(ptr, options.maxFileSize.toSizeT)

  nativeFilterPolicy.foreach(fp => leveldb_options_set_filter_policy(ptr, fp))
  nativeCache.foreach(c => leveldb_options_set_cache(ptr, c))

  override def close(): Unit = {
    nativeCache.foreach(leveldb_cache_destroy)
    nativeFilterPolicy.foreach(leveldb_filterpolicy_destroy)

    leveldb_options_destroy(ptr)
  }
}
