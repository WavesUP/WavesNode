package com.wavesplatform.leveldb

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

trait TestDB {
  def withDB[A](f: DB => A): A = withDB(Options())(f)

  def withDB[A](options: Options)(f: DB => A): A = {
    val dbPath = Files.createTempDirectory("ldbtest").toAbsolutePath
    try {
      val db = DB(dbPath, options.copy(createIfMissing = true))
      val result = f(db)
      db.close()
      result
    }
    finally TestDB.deleteRecursively(dbPath)

  }
}

object TestDB {
  def deleteRecursively(path: Path): Unit = Files.walkFileTree(
    path,
    new SimpleFileVisitor[Path] {
      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
        Option(exc).fold {
          Files.delete(dir)
          FileVisitResult.CONTINUE
        }(throw _)
      }

      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }
    }
  )
}
