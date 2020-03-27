package com.wavesplatform.leveldb.impl

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

object ByteBufferOps {
  def getUtf8String(buf: ByteBuffer): String = StandardCharsets.UTF_8.decode(buf).toString
  def ignore(buf: ByteBuffer): Unit = ()
}
