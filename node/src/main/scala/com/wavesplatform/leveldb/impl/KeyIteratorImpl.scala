package com.wavesplatform.leveldb.impl

import com.wavesplatform.leveldb.{KeyIterator, ReadOptions}

class KeyIteratorImpl(nativeDB: NativeLevelDB.LevelDB, nativeReadOptions: NativeReadOptions)
  extends SeekingIteratorBase[Array[Byte]](nativeDB, nativeReadOptions) with KeyIterator {
  def this(nativeDB: NativeLevelDB.LevelDB, readOptions: ReadOptions) = this(nativeDB, new NativeReadOptions(readOptions))

  override protected def currentValue(): Array[Byte] = {
    currentBytes(currentKey) { b =>
      val dest = new Array[Byte](b.remaining())
      b.get(dest)
      dest
    }
  }
}
