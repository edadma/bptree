package io.github.edadma.bptree

import java.io.File

def newfile: String =
  val f = File.createTempFile("testfile", ".btree")
  f.deleteOnExit()
  f.getPath
