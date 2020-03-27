package com.wavesplatform.leveldb.impl

import com.wavesplatform.leveldb.{ReadOptions, View, WriteOptions, impl}

class ViewImpl(nativeDB: NativeLevelDB.LevelDB, readOptions: ReadOptions, writeOptions: WriteOptions) extends View {
  private[this] val nativeReadOptions = new NativeReadOptions(readOptions)
  private[this] val nativeWriteOptions = new NativeWriteOptions(writeOptions)

  override val keyIterator = ???
  override val keyValueIterator = ???

  override def get(key: Array[Byte]): Option[Array[Byte]] = impl.get(nativeDB, nativeReadOptions.ptr, key)

  override def put(key: Array[Byte], value: Array[Byte]): Unit = ???

  override def delete(key: Array[Byte]): Unit = ???

  override def close(): Unit = {
    nativeReadOptions.close()
    nativeWriteOptions.close()
  }
}
