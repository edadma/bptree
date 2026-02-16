package io.github.edadma.bptree

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DotStringTests extends AnyFreeSpec with Matchers:

  "dotString produces valid DOT output for empty tree" in {
    val t = new MemoryBPlusTree[String, Any](3)
    val dot = t.dotString

    dot should include("digraph")
    dot should include("node [shape = record")
    dot should include("}")
  }

  "dotString produces valid DOT output for non-empty tree" in {
    val t = new MemoryBPlusTree[String, Any](3)

    t.insertKeys("c", "a", "b", "d", "e")
    val dot = t.dotString

    dot should include("digraph")
    dot should include("}")
    // internal nodes should have bull (bullet) references
    dot should include("&bull;")
    // leaf nodes should contain key labels
    dot should include("a")
    dot should include("e")
  }
end DotStringTests
