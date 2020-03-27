package com.wavesplatform.leveldb

import com.wavesplatform.leveldb.impl.ByteBufferOps
import org.scalactic.source.Position
import org.scalatest.OptionValues._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DBSpec extends AnyFreeSpec with Matchers with TestDB {

  import ByteBufferOps._

  private def putAndCheck(db: DB, key: String, value: String)(implicit pos: Position): Unit =
    putAndCheck(db, key.getBytes, value)

  private def putAndCheck(db: DB, key: Array[Byte], value: String)(implicit pos: Position): Unit = {
    db.put(key, value.getBytes)
    db.getString(key).value shouldEqual value
  }

  "EmptyKey" in withDB { db =>
    db.put(null, "v1".getBytes)
    db.getString(null).value shouldEqual "v1"
    db.put(Array.emptyByteArray, "v2".getBytes)
    db.getString(Array.emptyByteArray).value shouldEqual "v2"
  }

  "EmptyValue" in withDB { db =>
    val key = "key".getBytes
    db.put(key, "v1".getBytes)
    db.getString(key).value shouldEqual "v1"

    db.put(key, Array.emptyByteArray)
    db.getString(key).value shouldEqual ""

    db.put(key, null)
    db.getString(key).value shouldEqual ""

    db.put(key, "v2".getBytes)
    db.getString(key).value shouldEqual "v2"
  }

  "PutDeleteGet" in withDB { db =>
    val key = "KEY1".getBytes
    val value = "VALUE".getBytes
    db.put(key, value)
    db.getString(key).value shouldEqual "VALUE"
    db.delete(key)
    db.get(key)(ignore) shouldBe 'empty
  }

  private val writeBufferSize = 100000

  "GetFromImmutableLayer" in withDB(Options(writeBufferSize = writeBufferSize)) { db =>
    db.put("foo".getBytes, "v1".getBytes)
    db.getString("foo".getBytes).value shouldEqual "v1"

    db.put("k1".getBytes, ("x" * writeBufferSize).getBytes) // Fill memtable.
    db.put("k2".getBytes, ("y" * writeBufferSize).getBytes) // Trigger compaction.

    db.getString("foo".getBytes).value shouldEqual "v1"
  }

  "GetMemUsage" in withDB { db =>
    db.put("foo".getBytes, "v1".getBytes)
    val memUsage = db.property(Property.ApproximateMemoryUsage).toInt

    memUsage should be > 0
    memUsage should be < 5 * 1024 * 1025
  }

  "GetSnapshot" in withDB { db =>
    Seq("foo", "x" * 200).map(_.getBytes) foreach { key =>
      db.put(key, "v1".getBytes)
      val snapshot = db.snapshot()
      putAndCheck(db, key, "v2")
      db.getString(key, ReadOptions(snapshot = Some(snapshot))).value shouldEqual "v1"
      snapshot.close()
    }
  }

  "GetIdenticalSnapshots" in withDB { db =>
    Seq("foo", "x" * 200).map(_.getBytes).foreach { key =>
      db.put(key, "v1".getBytes)
      val s1 = db.snapshot()
      val s2 = db.snapshot()
      val s3 = db.snapshot()
      db.put(key, "v2".getBytes)

      db.getString(key).value shouldEqual "v2"
      db.getString(key, ReadOptions(snapshot = Some(s1))).value shouldEqual "v1"
      db.getString(key, ReadOptions(snapshot = Some(s2))).value shouldEqual "v1"
      db.getString(key, ReadOptions(snapshot = Some(s3))).value shouldEqual "v1"

      s1.close()

      db.getString(key).value shouldEqual "v2"
      db.getString(key, ReadOptions(snapshot = Some(s2))).value shouldEqual "v1"
      db.getString(key, ReadOptions(snapshot = Some(s3))).value shouldEqual "v1"

      s2.close()

      db.getString(key).value shouldEqual "v2"
      db.getString(key, ReadOptions(snapshot = Some(s3))).value shouldEqual "v1"

      s3.close()
    }
  }

  "IterateOverEmptySnapshot" in withDB { db =>
    val snapshot = db.snapshot()
    val key = "foo".getBytes
    putAndCheck(db, key, "v1")
    putAndCheck(db, key, "v2")

    val i1 = db.keyIterator()
    i1.seekToFirst()
    i1.hasNext should be (false)
    i1.close()
  }

  "IterEmpty" in()

  "IterSingle" in()

  "IterMulti" in()

  "IterSmallAndLargeMix" in()

  "IterMultiWithDelete" in()

  "Recover" in()

  "RecoveryWithEmptyLog" in()

  "RecoverDuringMemtableCompaction" in()

  "MinorCompactionsHappen" in()

  "RecoverWithLargeLog" in()

  "RepeatedWritesToSameKey" in()

  "IteratorPinsRef" in()

  "Snapshot" in()

  "HiddenValuesAreRemoved" in()

  "L0_CompactionBug_Issue44_a" in()

  "L0_CompactionBug_Issue44_b" in()

  "ManualCompaction" in()

  "DBOpen_Options" in()

  "DestroyEmptyDir" in()

  "BloomFilter" in()
}
