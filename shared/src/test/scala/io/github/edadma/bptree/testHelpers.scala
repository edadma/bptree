package io.github.edadma.bptree

import io.github.edadma.cross_platform.{createTempFile, openRandomAccessFile, deleteFile}

import org.scalatest.{Canceled, Outcome}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

def newfile: String = createTempFile("testfile", ".btree")

lazy val fileTestsSupported: Boolean =
  try
    val path = createTempFile("filetest-check", ".tmp")
    val raf = openRandomAccessFile(path, "rw")
    raf.setLength(16)
    raf.seek(0)
    raf.writeLong(12345L)
    raf.seek(0)
    val ok = raf.readLong() == 12345L && raf.length == 16
    raf.close()
    deleteFile(path)
    ok
  catch case _: Throwable => false

trait FileTestBase extends AnyFreeSpec with Matchers:
  override def withFixture(test: NoArgTest): Outcome =
    if fileTestsSupported then super.withFixture(test)
    else Canceled("File I/O not fully supported on this platform")
