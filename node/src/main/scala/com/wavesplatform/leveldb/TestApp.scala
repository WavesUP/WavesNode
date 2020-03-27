package com.wavesplatform.leveldb

import java.io.File
import java.nio.ByteBuffer

object TestApp {
  def main(args: Array[String]): Unit = {
    val db = DB(new File("/Users/phearnot/tmp/mainnet/data").toPath)
    val (major, minor) = db.version
    println(s"LevelDB version $major.$minor")
    println(db.property(Property.Stats))
    println(db.property(Property.ApproximateMemoryUsage))
    val bytes = db.get(Array(0.toByte, 1.toByte))(_.getInt)
    println(s"Blockchain height: ${bytes}")
    db.close()
  }
}
