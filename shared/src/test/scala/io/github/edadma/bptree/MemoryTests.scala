package io.github.edadma.bptree

import scala.util.Random

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Tag

object BasicTest extends Tag("basic")
object SlowTest extends Tag("slow")

class InsertionTests extends AnyFreeSpec with Matchers:

  "ascending insertion (order 3): in memory" in {
    val t = new MemoryBPlusTree[String, Any](3)

    t.prettyString shouldBe "[n0: (null, null, null)]"
    t.insert("a", 1)
    t.prettyStringWithValues shouldBe "[n0: (null, null, null) <a, 1>]"
    t.insert("b", 2)
    t.prettyStringWithValues shouldBe "[n0: (null, null, null) <a, 1> <b, 2>]"
    t.insert("c", 3)
    t.prettyStringWithValues shouldBe
      """|[n0: (null, null, null) n1 | b | n2]
         |[n1: (null, n0, n2) <a, 1>] [n2: (n1, n0, null) <b, 2> <c, 3>]""".stripMargin
    t.insert("d", 4)
    t.prettyStringWithValues shouldBe
      """|[n0: (null, null, null) n1 | b | n2 | c | n3]
         |[n1: (null, n0, n2) <a, 1>] [n2: (n1, n0, n3) <b, 2>] [n3: (n2, n0, null) <c, 3> <d, 4>]""".stripMargin
    t.insert("e", 5)
    t.prettyStringWithValues shouldBe
      """|[n0: (null, null, null) n1 | c | n2]
         |[n1: (null, n0, n2) n3 | b | n4] [n2: (n1, n0, null) n5 | d | n6]
         |[n3: (null, n1, n4) <a, 1>] [n4: (n3, n1, n5) <b, 2>] [n5: (n4, n2, n6) <c, 3>] [n6: (n5, n2, null) <d, 4> <e, 5>]""".stripMargin
    t.insert("f", 6)
    t.prettyStringWithValues shouldBe
      """|[n0: (null, null, null) n1 | c | n2]
         |[n1: (null, n0, n2) n3 | b | n4] [n2: (n1, n0, null) n5 | d | n6 | e | n7]
         |[n3: (null, n1, n4) <a, 1>] [n4: (n3, n1, n5) <b, 2>] [n5: (n4, n2, n6) <c, 3>] [n6: (n5, n2, n7) <d, 4>] [n7: (n6, n2, null) <e, 5> <f, 6>]""".stripMargin
    t.insert("g", 7)
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | c | n2 | e | n3]
         |[n1: (null, n0, n2) n4 | b | n5] [n2: (n1, n0, n3) n6 | d | n7] [n3: (n2, n0, null) n8 | f | n9]
         |[n4: (null, n1, n5) a] [n5: (n4, n1, n6) b] [n6: (n5, n2, n7) c] [n7: (n6, n2, n8) d] [n8: (n7, n3, n9) e] [n9: (n8, n3, null) f g]""".stripMargin
    t.wellConstructed shouldBe "true"
    t.prettySearch("c") shouldBe "n6 3 0"
  }

  "descending insertion (order 3): in memory" in {
    val t = new MemoryBPlusTree[String, Any](3)

    t.insertKeysAndCheck("g", "f") shouldBe "true"
    t.prettyString shouldBe "[n0: (null, null, null) f g]"
    t.insert("e")
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | f | n2]
         |[n1: (null, n0, n2) e] [n2: (n1, n0, null) f g]""".stripMargin
    t.insert("d")
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | f | n2]
         |[n1: (null, n0, n2) d e] [n2: (n1, n0, null) f g]""".stripMargin
    t.insert("c")
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | d | n2 | f | n3]
         |[n1: (null, n0, n2) c] [n2: (n1, n0, n3) d e] [n3: (n2, n0, null) f g]""".stripMargin
    t.insert("b")
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | d | n2 | f | n3]
         |[n1: (null, n0, n2) b c] [n2: (n1, n0, n3) d e] [n3: (n2, n0, null) f g]""".stripMargin
    t.insert("a")
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | d | n2]
         |[n1: (null, n0, n2) n3 | b | n4] [n2: (n1, n0, null) n5 | f | n6]
         |[n3: (null, n1, n4) a] [n4: (n3, n1, n5) b c] [n5: (n4, n2, n6) d e] [n6: (n5, n2, null) f g]""".stripMargin
    t.wellConstructed shouldBe "true"
    t.prettySearch("e") shouldBe "n5 null 1"
  }

  "random insertion (order 3): in memory" in {
    val t = new MemoryBPlusTree[String, Any](3)

    t.insertKeysAndCheck("v", "t", "u", "j", "g", "w", "y", "c", "n", "l", "a", "r", "b", "s", "e", "f", "i", "z", "h", "d", "p", "x", "m", "k", "o", "q") shouldBe "true"
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | g | n2 | r | n3]
         |[n1: (null, n0, n2) n4 | e | n5] [n2: (n1, n0, n3) n6 | j | n7 | n | n8] [n3: (n2, n0, null) n9 | u | n10 | w | n11]
         |[n4: (null, n1, n5) n12 | c | n13] [n5: (n4, n1, null) n14 | f | n15] [n6: (null, n2, n7) n16 | h | n17] [n7: (n6, n2, n8) n18 | l | n19] [n8: (n7, n2, null) n20 | o | n21 | p | n22] [n9: (null, n3, n10) n23 | s | n24] [n10: (n9, n3, n11) n25 | v | n26] [n11: (n10, n3, null) n27 | y | n28]
         |[n12: (null, n4, n13) a b] [n13: (n12, n4, n14) c d] [n14: (n13, n5, n15) e] [n15: (n14, n5, n16) f] [n16: (n15, n6, n17) g] [n17: (n16, n6, n18) h i] [n18: (n17, n7, n19) j k] [n19: (n18, n7, n20) l m] [n20: (n19, n8, n21) n] [n21: (n20, n8, n22) o] [n22: (n21, n8, n23) p q] [n23: (n22, n9, n24) r] [n24: (n23, n9, n25) s t] [n25: (n24, n10, n26) u] [n26: (n25, n10, n27) v] [n27: (n26, n11, n28) w x] [n28: (n27, n11, null) y z]""".stripMargin
    t.prettySearch("i") shouldBe "n17 null 1"
  }

  "ascending insertion (order 4): in memory" in {
    val t = new MemoryBPlusTree[String, Any](4)

    t.prettyString shouldBe "[n0: (null, null, null)]"
    t.insert("a")
    t.insert("b")
    t.insert("c")
    t.insert("d")
    t.insert("e")
    t.insert("f")
    t.insert("g")
    t.insert("h")
    t.insert("i")
    t.insert("j")
    t.insert("k")
    t.insert("l")
    t.insert("m")
    t.insert("n")
    t.insert("o")
    t.insert("p")
    t.wellConstructed shouldBe "true"
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | g | n2 | m | n3]
         |[n1: (null, n0, n2) n4 | c | n5 | e | n6] [n2: (n1, n0, n3) n7 | i | n8 | k | n9] [n3: (n2, n0, null) n10 | o | n11]
         |[n4: (null, n1, n5) a b] [n5: (n4, n1, n6) c d] [n6: (n5, n1, n7) e f] [n7: (n6, n2, n8) g h] [n8: (n7, n2, n9) i j] [n9: (n8, n2, n10) k l] [n10: (n9, n3, n11) m n] [n11: (n10, n3, null) o p]""".stripMargin
    t.prettySearch("h") shouldBe "n7 null 1"
  }

  "descending insertion (order 4): in memory" in {
    val t = new MemoryBPlusTree[String, Any](4)

    t.insertKeysAndCheck("p", "o", "n", "m", "l", "k", "j", "i", "h", "g", "f", "e", "d", "c", "b", "a") shouldBe "true"
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | i | n2 | m | n3]
         |[n1: (null, n0, n2) n4 | c | n5 | e | n6 | g | n7] [n2: (n1, n0, n3) n8 | k | n9] [n3: (n2, n0, null) n10 | o | n11]
         |[n4: (null, n1, n5) a b] [n5: (n4, n1, n6) c d] [n6: (n5, n1, n7) e f] [n7: (n6, n1, n8) g h] [n8: (n7, n2, n9) i j] [n9: (n8, n2, n10) k l] [n10: (n9, n3, n11) m n] [n11: (n10, n3, null) o p]""".stripMargin
    t.prettySearch("h") shouldBe "n7 null 1"
  }

  "random insertion (order 4): in memory" in {
    val t = new MemoryBPlusTree[String, Any](4)

    t.insertKeysAndCheck("v", "t", "u", "j", "g", "w", "y", "c", "n", "l", "a", "r", "b", "s", "e", "f", "i", "z", "h", "d", "p", "x", "m", "k", "o", "q") shouldBe "true"
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | h | n2 | n | n3 | u | n4]
         |[n1: (null, n0, n2) n5 | c | n6 | f | n7] [n2: (n1, n0, n3) n8 | j | n9 | l | n10] [n3: (n2, n0, n4) n11 | p | n12 | s | n13] [n4: (n3, n0, null) n14 | w | n15 | y | n16]
         |[n5: (null, n1, n6) a b] [n6: (n5, n1, n7) c d e] [n7: (n6, n1, n8) f g] [n8: (n7, n2, n9) h i] [n9: (n8, n2, n10) j k] [n10: (n9, n2, n11) l m] [n11: (n10, n3, n12) n o] [n12: (n11, n3, n13) p q r] [n13: (n12, n3, n14) s t] [n14: (n13, n4, n15) u v] [n15: (n14, n4, n16) w x] [n16: (n15, n4, null) y z]""".stripMargin
    t.prettySearch("i") shouldBe "n8 null 1"
  }
end InsertionTests

class DeletionTests extends AnyFreeSpec with Matchers:

  "deletion (leaf merge, 2 level tree, first): in memory, order 3" in {
    val t = new MemoryBPlusTree[String, Any](3)

    t.build("""
    (
      [a] b [b c]
    )
    """).prettyString shouldBe
      """|[n0: (null, null, null) n1 | b | n2]
         |[n1: (null, n0, n2) a] [n2: (n1, n0, null) b c]""".stripMargin
    t.delete("b") shouldBe true
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | b | n2]
         |[n1: (null, n0, n2) a] [n2: (n1, n0, null) c]""".stripMargin
    t.wellConstructed shouldBe "true"
  }

  "deletion (leaf merge, 2 level tree, second): in memory, order 3" in {
    val t = new MemoryBPlusTree[String, Any](3)

    t.build("""
    (
      [a] b [b c]
    )
    """)

    t.delete("c") shouldBe true
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | b | n2]
         |[n1: (null, n0, n2) a] [n2: (n1, n0, null) b]""".stripMargin
    t.wellConstructed shouldBe "true"
    t.delete("b") shouldBe true
    t.prettyString shouldBe "[n0: (null, null, null) a]"
    t.wellConstructed shouldBe "true"
  }

  "deletion (leaf merge, 3 level tree, first): in memory, order 3" in {
    val t = new MemoryBPlusTree[String, Any](3)

    t.build("""
    (
      ([a] b [b c]) d ([d] e [e])
    )
    """).prettyString shouldBe
      """|[n0: (null, null, null) n1 | d | n2]
         |[n1: (null, n0, n2) n3 | b | n4] [n2: (n1, n0, null) n5 | e | n6]
         |[n3: (null, n1, n4) a] [n4: (n3, n1, n5) b c] [n5: (n4, n2, n6) d] [n6: (n5, n2, null) e]""".stripMargin
    t.delete("b") shouldBe true
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | d | n2]
         |[n1: (null, n0, n2) n3 | b | n4] [n2: (n1, n0, null) n5 | e | n6]
         |[n3: (null, n1, n4) a] [n4: (n3, n1, n5) c] [n5: (n4, n2, n6) d] [n6: (n5, n2, null) e]""".stripMargin
    t.wellConstructed shouldBe "true"
    t.delete("a") shouldBe true
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | d | n2 | e | n3]
         |[n1: (null, n0, n2) c] [n2: (n1, n0, n3) d] [n3: (n2, n0, null) e]""".stripMargin
    t.wellConstructed shouldBe "true"
    t.delete("c") shouldBe true
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | e | n2]
         |[n1: (null, n0, n2) d] [n2: (n1, n0, null) e]""".stripMargin
    t.wellConstructed shouldBe "true"
    t.delete("d") shouldBe true
    t.prettyString shouldBe "[n0: (null, null, null) e]"
    t.wellConstructed shouldBe "true"
    t.delete("e") shouldBe true
    t.prettyString shouldBe "[n0: (null, null, null)]"
    t.wellConstructed shouldBe "true"
  }

  "deletion (leaf merge, 3 level tree, second): in memory, order 3" in {
    val t = new MemoryBPlusTree[String, Any](3)

    t.build("""
    (
      ([a] b [b]) d ([d] e [e])
    )
    """).prettyString shouldBe
      """|[n0: (null, null, null) n1 | d | n2]
         |[n1: (null, n0, n2) n3 | b | n4] [n2: (n1, n0, null) n5 | e | n6]
         |[n3: (null, n1, n4) a] [n4: (n3, n1, n5) b] [n5: (n4, n2, n6) d] [n6: (n5, n2, null) e]""".stripMargin
    t.delete("b") shouldBe true
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | d | n2 | e | n3]
         |[n1: (null, n0, n2) a] [n2: (n1, n0, n3) d] [n3: (n2, n0, null) e]""".stripMargin
    t.wellConstructed shouldBe "true"
  }

  "deletion (leaf merge, 3 level tree, third): in memory, order 3" in {
    val t = new MemoryBPlusTree[String, Any](3)

    t.build("""
    (
      ([a] b [b]) d ([d] e [e])
    )
    """)
    t.delete("d") shouldBe true
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | b | n2 | e | n3]
         |[n1: (null, n0, n2) a] [n2: (n1, n0, n3) b] [n3: (n2, n0, null) e]""".stripMargin
    t.wellConstructed shouldBe "true"
    t.delete("e") shouldBe true
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | b | n2]
         |[n1: (null, n0, n2) a] [n2: (n1, n0, null) b]""".stripMargin
    t.wellConstructed shouldBe "true"
  }

  "deletion (leaf merge, 3 level tree, fourth): in memory, order 3" in {
    val t = new MemoryBPlusTree[String, Any](3)

    t.build("""
    (
      ([a] b [b]) d ([d] e [e])
    )
    """)
    t.delete("d") shouldBe true
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | b | n2 | e | n3]
         |[n1: (null, n0, n2) a] [n2: (n1, n0, n3) b] [n3: (n2, n0, null) e]""".stripMargin
    t.wellConstructed shouldBe "true"
  }

  "deletion (leaf merge, 3 level tree, merge up to root - not empty): in memory, order 3" in {
    val t = new MemoryBPlusTree[Int, Any](3)

    t.build("""
    (
      ([1] 2 [2]) 3 ([3] 4 [4]) 5 ([5] 6 [6 7])
    )
    """).prettyString shouldBe
      """|[n0: (null, null, null) n1 | 3 | n2 | 5 | n3]
         |[n1: (null, n0, n2) n4 | 2 | n5] [n2: (n1, n0, n3) n6 | 4 | n7] [n3: (n2, n0, null) n8 | 6 | n9]
         |[n4: (null, n1, n5) 1] [n5: (n4, n1, n6) 2] [n6: (n5, n2, n7) 3] [n7: (n6, n2, n8) 4] [n8: (n7, n3, n9) 5] [n9: (n8, n3, null) 6 7]""".stripMargin
    t.delete(4) shouldBe true
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | 3 | n2]
         |[n1: (null, n0, n2) n3 | 2 | n4] [n2: (n1, n0, null) n5 | 5 | n6 | 6 | n7]
         |[n3: (null, n1, n4) 1] [n4: (n3, n1, n5) 2] [n5: (n4, n2, n6) 3] [n6: (n5, n2, n7) 5] [n7: (n6, n2, null) 6 7]""".stripMargin
    t.wellConstructed shouldBe "true"
  }

  "deletion (non-leaf borrow from right): in memory, order 3" in {
    val t = new MemoryBPlusTree[Int, Any](3)

    t.build("""
    (
      ([4] 5 [5]) 6 ([6] 8 [8] 9 [9])
    )
    """)
    t.delete(4) shouldBe true
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | 8 | n2]
         |[n1: (null, n0, n2) n3 | 6 | n4] [n2: (n1, n0, null) n5 | 9 | n6]
         |[n3: (null, n1, n4) 5] [n4: (n3, n1, n5) 6] [n5: (n4, n2, n6) 8] [n6: (n5, n2, null) 9]""".stripMargin
    t.wellConstructed shouldBe "true"
  }

  "deletion (non-leaf borrow from left): in memory, order 3" in {
    val t = new MemoryBPlusTree[Int, Any](3)

    t.build("""
    (
      ([4] 5 [5] 6 [6]) 8 ([8] 9 [9])
    )
    """)
    t.delete(8) shouldBe true
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | 6 | n2]
         |[n1: (null, n0, n2) n3 | 5 | n4] [n2: (n1, n0, null) n5 | 9 | n6]
         |[n3: (null, n1, n4) 4] [n4: (n3, n1, n5) 5] [n5: (n4, n2, n6) 6] [n6: (n5, n2, null) 9]""".stripMargin
    t.wellConstructed shouldBe "true"
  }

  "deletion (merge up to root, 4 level tree): in memory, order 3" in {
    val t = new MemoryBPlusTree[Int, Any](3)

    t.build("""
    (
      (
        ([1 2] 3 [3]) 4 ([4] 5 [5])
      )
      7
      (
        ([7] 8 [8 9]) 10 ([10] 11 [11 12])
      )
    )
    """)
    t.delete(5) shouldBe true
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | 7 | n2 | 10 | n3]
         |[n1: (null, n0, n2) n4 | 3 | n5 | 4 | n6] [n2: (n1, n0, n3) n7 | 8 | n8] [n3: (n2, n0, null) n9 | 11 | n10]
         |[n4: (null, n1, n5) 1 2] [n5: (n4, n1, n6) 3] [n6: (n5, n1, n7) 4] [n7: (n6, n2, n8) 7] [n8: (n7, n2, n9) 8 9] [n9: (n8, n3, n10) 10] [n10: (n9, n3, null) 11 12]""".stripMargin
    t.wellConstructed shouldBe "true"
  }

  "deletion (merge not up to root, 3 level tree): in memory, order 3" in {
    val t = new MemoryBPlusTree[Int, Any](3)

    t.build("""
    (
      ([1] 2 [2]) 6 ([6 7] 8 [8 9]) 10 ([11] 12 [12])
    )
    """)
    t.delete(2) shouldBe true
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | 10 | n2]
         |[n1: (null, n0, n2) n3 | 6 | n4 | 8 | n5] [n2: (n1, n0, null) n6 | 12 | n7]
         |[n3: (null, n1, n4) 1] [n4: (n3, n1, n5) 6 7] [n5: (n4, n1, n6) 8 9] [n6: (n5, n2, n7) 11] [n7: (n6, n2, null) 12]""".stripMargin
    t.wellConstructed shouldBe "true"
  }

  "deletion (merge followed by borrow, 4 level tree): in memory, order 3" in {
    val t = new MemoryBPlusTree[Int, Any](3)

    t.build("""
    (
      (
        ([1] 2 [2 3]) 4 ([5] 7 [7]) 8 ([8] 9 [9])
      )
      11
      (
        ([12] 13 [13]) 14 ([14 15] 16 [16 17])
      )
    )
    """)
    t.delete(13) shouldBe true
    t.prettyString shouldBe
      """|[n0: (null, null, null) n1 | 8 | n2]
         |[n1: (null, n0, n2) n3 | 4 | n4] [n2: (n1, n0, null) n5 | 12 | n6]
         |[n3: (null, n1, n4) n7 | 2 | n8] [n4: (n3, n1, null) n9 | 7 | n10] [n5: (null, n2, n6) n11 | 9 | n12] [n6: (n5, n2, null) n13 | 14 | n14 | 16 | n15]
         |[n7: (null, n3, n8) 1] [n8: (n7, n3, n9) 2 3] [n9: (n8, n4, n10) 5] [n10: (n9, n4, n11) 7] [n11: (n10, n5, n12) 8] [n12: (n11, n5, n13) 9] [n13: (n12, n6, n14) 12] [n14: (n13, n6, n15) 14 15] [n15: (n14, n6, null) 16 17]""".stripMargin
    t.wellConstructed shouldBe "true"
  }
end DeletionTests

class MiscTests extends AnyFreeSpec with Matchers:

  "max/min type methods" in {
    val t = new MemoryBPlusTree[Int, String](3)

    t.min shouldBe None
    t.max shouldBe None
    t.minKey shouldBe None
    t.maxKey shouldBe None
    t.leastGreaterThanOrEqual(3) shouldBe None
    t.leastGreaterThan(3) shouldBe None
    t.greatestLessThanOrEqual(3) shouldBe None
    t.greatestLessThan(3) shouldBe None
    t.insert(5, "five")
    t.min shouldBe Some((5, "five"))
    t.max shouldBe Some((5, "five"))
    t.minKey shouldBe Some(5)
    t.maxKey shouldBe Some(5)

    t.leastGreaterThanOrEqual(3) shouldBe Some((5, "five"))
    t.leastGreaterThan(3) shouldBe Some((5, "five"))
    t.leastGreaterThanOrEqual(5) shouldBe Some((5, "five"))
    t.leastGreaterThan(5) shouldBe None
    t.leastGreaterThanOrEqual(6) shouldBe None
    t.leastGreaterThan(6) shouldBe None
    t.greatestLessThanOrEqual(3) shouldBe None
    t.greatestLessThan(3) shouldBe None
    t.greatestLessThanOrEqual(5) shouldBe Some((5, "five"))
    t.greatestLessThan(5) shouldBe None
    t.greatestLessThanOrEqual(6) shouldBe Some((5, "five"))
    t.greatestLessThan(6) shouldBe Some((5, "five"))
    t.insert(3, "three")
    t.min shouldBe Some((3, "three"))
    t.max shouldBe Some((5, "five"))
    t.minKey shouldBe Some(3)
    t.maxKey shouldBe Some(5)
    t.leastGreaterThanOrEqual(2) shouldBe Some((3, "three"))
    t.leastGreaterThan(2) shouldBe Some((3, "three"))
    t.leastGreaterThanOrEqual(3) shouldBe Some((3, "three"))
    t.leastGreaterThan(3) shouldBe Some((5, "five"))
    t.leastGreaterThanOrEqual(5) shouldBe Some((5, "five"))
    t.leastGreaterThan(5) shouldBe None
    t.leastGreaterThanOrEqual(6) shouldBe None
    t.leastGreaterThan(6) shouldBe None

    t.greatestLessThanOrEqual(2) shouldBe None
    t.greatestLessThan(2) shouldBe None
    t.greatestLessThanOrEqual(3) shouldBe Some((3, "three"))
    t.greatestLessThan(3) shouldBe None
    t.greatestLessThanOrEqual(5) shouldBe Some((5, "five"))
    t.greatestLessThan(5) shouldBe Some((3, "three"))
    t.greatestLessThanOrEqual(6) shouldBe Some((5, "five"))
    t.greatestLessThan(6) shouldBe Some((5, "five"))
  }
end MiscTests

class MultiOrderTests extends AnyFreeSpec with Matchers:

  val memoryOrders = List(3, 4, 5, 6)

  for order <- memoryOrders do
    s"iterator: in memory, order $order" in {
      val t = new MemoryBPlusTree[String, Any](order)

      t.iterator.isEmpty shouldBe true
      t.reverseIterator.isEmpty shouldBe true
      t.boundedIterator((Bound.Gte, "a")).isEmpty shouldBe true
      t.boundedIterator((Bound.Gte, "a"), (Bound.Lte, "z")).isEmpty shouldBe true
      t.boundedIterator((Bound.Lte, "z")).isEmpty shouldBe true
      t.insertKeysAndCheck("v", "t", "u", "j", "g", "w", "y", "c", "n", "a", "r", "b", "s", "e", "f", "i", "z", "d", "p", "x", "m", "k", "o", "q") shouldBe "true"
      t.keysIterator.mkString shouldBe "abcdefgijkmnopqrstuvwxyz"
      t.reverseKeysIterator.mkString shouldBe "zyxwvutsrqponmkjigfedcba"
      t.boundedKeysIterator((Bound.Gte, "a")).mkString shouldBe "abcdefgijkmnopqrstuvwxyz"
      t.boundedKeysIterator((Bound.Gt, "a")).mkString shouldBe "bcdefgijkmnopqrstuvwxyz"
      t.boundedKeysIterator((Bound.Gte, "A")).mkString shouldBe "abcdefgijkmnopqrstuvwxyz"
      t.boundedKeysIterator((Bound.Gt, "A")).mkString shouldBe "abcdefgijkmnopqrstuvwxyz"
      t.boundedKeysIterator((Bound.Lte, "z")).mkString shouldBe "abcdefgijkmnopqrstuvwxyz"
      t.boundedKeysIterator((Bound.Lt, "z")).mkString shouldBe "abcdefgijkmnopqrstuvwxy"
      t.boundedKeysIterator((Bound.Lte, "{")).mkString shouldBe "abcdefgijkmnopqrstuvwxyz"
      t.boundedKeysIterator((Bound.Lt, "{")).mkString shouldBe "abcdefgijkmnopqrstuvwxyz"
      t.boundedKeysIterator((Bound.Gte, "a"), (Bound.Lte, "d")).mkString shouldBe "abcd"
      t.boundedKeysIterator((Bound.Gt, "a"), (Bound.Lte, "d")).mkString shouldBe "bcd"
      t.boundedKeysIterator((Bound.Gte, "a"), (Bound.Lt, "d")).mkString shouldBe "abc"
      t.boundedKeysIterator((Bound.Gt, "a"), (Bound.Lt, "d")).mkString shouldBe "bc"
      t.boundedKeysIterator((Bound.Lt, "d"), (Bound.Gt, "a")).mkString shouldBe "bc"
      t.boundedKeysIterator((Bound.Gt, "a"), (Bound.Lt, "a")).mkString shouldBe ""
      t.boundedKeysIterator((Bound.Gte, "a"), (Bound.Lt, "a")).mkString shouldBe ""
      t.boundedKeysIterator((Bound.Gt, "a"), (Bound.Lte, "a")).mkString shouldBe ""
      t.boundedKeysIterator((Bound.Gte, "a"), (Bound.Lte, "a")).mkString shouldBe "a"
      t.boundedKeysIterator((Bound.Gte, "c"), (Bound.Lte, "a")).mkString shouldBe ""
      t.boundedKeysIterator((Bound.Gt, "c"), (Bound.Lte, "a")).mkString shouldBe ""
      t.boundedKeysIterator((Bound.Gte, "c"), (Bound.Lt, "a")).mkString shouldBe ""
      t.boundedKeysIterator((Bound.Gt, "c"), (Bound.Lt, "a")).mkString shouldBe ""
      t.boundedKeysIterator((Bound.Gt, "h"), (Bound.Lt, "h")).mkString shouldBe ""
      t.boundedKeysIterator((Bound.Gte, "h"), (Bound.Lt, "h")).mkString shouldBe ""
      t.boundedKeysIterator((Bound.Gt, "h"), (Bound.Lte, "h")).mkString shouldBe ""
      t.boundedKeysIterator((Bound.Gte, "h"), (Bound.Lte, "h")).mkString shouldBe ""
      t.boundedKeysIterator((Bound.Gte, "h"), (Bound.Lte, "a")).mkString shouldBe ""
      t.boundedKeysIterator((Bound.Gt, "h"), (Bound.Lte, "a")).mkString shouldBe ""
      t.boundedKeysIterator((Bound.Gte, "h"), (Bound.Lt, "a")).mkString shouldBe ""
      t.boundedKeysIterator((Bound.Gt, "h"), (Bound.Lt, "a")).mkString shouldBe ""
      t.boundedKeysIterator((Bound.Gte, "k"), (Bound.Lte, "h")).mkString shouldBe ""
      t.boundedKeysIterator((Bound.Gt, "k"), (Bound.Lte, "h")).mkString shouldBe ""
      t.boundedKeysIterator((Bound.Gte, "k"), (Bound.Lt, "h")).mkString shouldBe ""
      t.boundedKeysIterator((Bound.Gt, "k"), (Bound.Lt, "h")).mkString shouldBe ""
      t.boundedKeysIterator((Bound.Gte, "h"), (Bound.Lte, "l")).mkString shouldBe "ijk"
      t.boundedKeysIterator((Bound.Gt, "h"), (Bound.Lte, "l")).mkString shouldBe "ijk"
      t.boundedKeysIterator((Bound.Gte, "h"), (Bound.Lt, "l")).mkString shouldBe "ijk"
      t.boundedKeysIterator((Bound.Gt, "h"), (Bound.Lt, "l")).mkString shouldBe "ijk"
    }

  for order <- memoryOrders do
    s"bulk loading: in memory, order $order" in {
      val t = new MemoryBPlusTree[String, Any](order)

      t.load(("h", 8), ("i", 9), ("d", 4), ("b", 2), ("j", 10), ("f", 6), ("g", 7), ("a", 1), ("c", 3), ("e", 5))
      t.wellConstructed shouldBe "true"
      t.iterator.mkString(", ") shouldBe "(a,1), (b,2), (c,3), (d,4), (e,5), (f,6), (g,7), (h,8), (i,9), (j,10)"
      t.load(("p", 16), ("r", 18), ("l", 12), ("k", 11), ("o", 15), ("s", 19), ("q", 17), ("t", 20), ("m", 13), ("n", 14))
      t.wellConstructed shouldBe "true"
      t.iterator.mkString(", ") shouldBe "(a,1), (b,2), (c,3), (d,4), (e,5), (f,6), (g,7), (h,8), (i,9), (j,10), (k,11), (l,12), (m,13), (n,14), (o,15), (p,16), (q,17), (r,18), (s,19), (t,20)"
      a[RuntimeException] should be thrownBy { t.load(("A", 0)) }
    }
end MultiOrderTests

class StressTests extends AnyFreeSpec with Matchers:

  val memoryTests = List(
    (3, 50),
    (4, 50),
    (5, 50),
    (6, 50),
    (49, 500),
    (50, 500),
  )

  for (order, size) <- memoryTests do
    s"random insertion/deletion: in memory, $order, $size" in {
      val t = new MemoryBPlusTree[Int, Any](order)

      t.insertKeysAndCheck(Random.shuffle(1 to size)*) shouldBe "true"

      for k <- Random.shuffle(1 to size) do
        t.delete(k) shouldBe true
        t.wellConstructed shouldBe "true"

      t.isEmpty shouldBe true
      t.iterator.isEmpty shouldBe true
    }

  for (order, size) <- memoryTests do
    s"ascending insertion/deletion: in memory, $order, $size" in {
      val t = new MemoryBPlusTree[Int, Any](order)

      t.insertKeysAndCheck((1 to size)*) shouldBe "true"

      for k <- Random.shuffle(1 to size) do
        t.delete(k) shouldBe true
        t.wellConstructed shouldBe "true"

      t.isEmpty shouldBe true
      t.iterator.isEmpty shouldBe true
    }

  for (order, size) <- memoryTests do
    s"random insertion/deletion twice: in memory, $order, $size" in {
      val t = new MemoryBPlusTree[Int, Any](order)

      t.insertKeysAndCheck(Random.shuffle(1 to size)*) shouldBe "true"

      for k <- 1 to size / 2 do
        t.delete(k) shouldBe true
        t.wellConstructed shouldBe "true"

      t.insertKeysAndCheck(Random.shuffle(size + 1 to 2 * size)*) shouldBe "true"

      for k <- size / 2 + 1 to size do
        t.delete(k) shouldBe true
        t.wellConstructed shouldBe "true"

      t.keysIterator.toList == (size + 1 to 2 * size) shouldBe true
    }
end StressTests

class ReverseBoundedIteratorTests extends AnyFreeSpec with Matchers:

  "reverseBoundedIterator on empty tree" in {
    val t = new MemoryBPlusTree[String, Any](3)

    t.reverseBoundedIterator((Bound.Gte, "a")).isEmpty shouldBe true
    t.reverseBoundedIterator((Bound.Gte, "a"), (Bound.Lte, "z")).isEmpty shouldBe true
  }

  "reverseBoundedIterator with all bound combinations" in {
    val t = new MemoryBPlusTree[String, Int](5)

    for (c, i) <- "abcdefghij".zipWithIndex do t.insert(c.toString, i + 1)

    // single lower bound
    t.reverseBoundedIterator((Bound.Gte, "c")).map(_._1).mkString shouldBe "jihgfedcba".filter(_ >= 'c')
    t.reverseBoundedIterator((Bound.Gt, "c")).map(_._1).mkString shouldBe "jihgfed"

    // single upper bound
    t.reverseBoundedIterator((Bound.Lte, "g")).map(_._1).mkString shouldBe "gfedcba"
    t.reverseBoundedIterator((Bound.Lt, "g")).map(_._1).mkString shouldBe "fedcba"

    // two bounds - results in descending order
    t.reverseBoundedIterator((Bound.Gte, "c"), (Bound.Lte, "g")).map(_._1).mkString shouldBe "gfedc"
    t.reverseBoundedIterator((Bound.Gt, "c"), (Bound.Lte, "g")).map(_._1).mkString shouldBe "gfed"
    t.reverseBoundedIterator((Bound.Gte, "c"), (Bound.Lt, "g")).map(_._1).mkString shouldBe "fedc"
    t.reverseBoundedIterator((Bound.Gt, "c"), (Bound.Lt, "g")).map(_._1).mkString shouldBe "fed"

    // reversed argument order should work the same
    t.reverseBoundedIterator((Bound.Lt, "g"), (Bound.Gt, "c")).map(_._1).mkString shouldBe "fed"

    // values should come through correctly
    t.reverseBoundedIterator((Bound.Gte, "c"), (Bound.Lte, "e")).toList shouldBe List(("e", 5), ("d", 4), ("c", 3))

    // empty ranges
    t.reverseBoundedIterator((Bound.Gt, "a"), (Bound.Lt, "a")).isEmpty shouldBe true
    t.reverseBoundedIterator((Bound.Gte, "g"), (Bound.Lte, "c")).isEmpty shouldBe true
  }

  "reverseBoundedKeysIterator" in {
    val t = new MemoryBPlusTree[Int, String](4)

    for i <- 1 to 20 do t.insert(i, s"val$i")

    t.reverseBoundedKeysIterator((Bound.Gte, 5), (Bound.Lte, 10)).toList shouldBe List(10, 9, 8, 7, 6, 5)
    t.reverseBoundedKeysIterator((Bound.Gt, 5), (Bound.Lt, 10)).toList shouldBe List(9, 8, 7, 6)
    t.reverseBoundedKeysIterator((Bound.Lte, 3)).toList shouldBe List(3, 2, 1)
    t.reverseBoundedKeysIterator((Bound.Gte, 18)).toList shouldBe List(20, 19, 18)
  }

  "reverseBoundedValuesIterator" in {
    val t = new MemoryBPlusTree[Int, String](4)

    for i <- 1 to 10 do t.insert(i, s"v$i")

    t.reverseBoundedValuesIterator((Bound.Gte, 3), (Bound.Lte, 6)).toList shouldBe List("v6", "v5", "v4", "v3")
    t.reverseBoundedValuesIterator((Bound.Lt, 4)).toList shouldBe List("v3", "v2", "v1")
  }
end ReverseBoundedIteratorTests

class InsertIfNotFoundTests extends AnyFreeSpec with Matchers:

  "insertIfNotFound inserts when key is absent" in {
    val t = new MemoryBPlusTree[String, Int](3)

    t.insertIfNotFound("a", 1) shouldBe false
    t.search("a") shouldBe Some(1)
    t.wellConstructed shouldBe "true"
  }

  "insertIfNotFound does not overwrite when key exists" in {
    val t = new MemoryBPlusTree[String, Int](3)

    t.insert("a", 1)
    t.insertIfNotFound("a", 99) shouldBe true
    t.search("a") shouldBe Some(1)
  }

  "insertIfNotFound with multiple keys" in {
    val t = new MemoryBPlusTree[Int, String](4)

    t.insertIfNotFound(5, "five") shouldBe false
    t.insertIfNotFound(3, "three") shouldBe false
    t.insertIfNotFound(7, "seven") shouldBe false
    t.insertIfNotFound(5, "FIVE") shouldBe true
    t.insertIfNotFound(3, "THREE") shouldBe true

    t.search(5) shouldBe Some("five")
    t.search(3) shouldBe Some("three")
    t.search(7) shouldBe Some("seven")
    t.wellConstructed shouldBe "true"
  }

  "insertIfNotFound with default null value" in {
    val t = new MemoryBPlusTree[String, String](3)

    t.insertIfNotFound("x") shouldBe false
    t.search("x") shouldBe Some(null)
    t.insertIfNotFound("x") shouldBe true
  }
end InsertIfNotFoundTests

class SearchAndDeleteEdgeCaseTests extends AnyFreeSpec with Matchers:

  "search returns None for non-existent key in non-empty tree" in {
    val t = new MemoryBPlusTree[Int, String](3)

    t.insert(1, "one")
    t.insert(3, "three")
    t.insert(5, "five")
    t.search(2) shouldBe None
    t.search(4) shouldBe None
    t.search(0) shouldBe None
    t.search(6) shouldBe None
  }

  "search returns None on empty tree" in {
    val t = new MemoryBPlusTree[String, Int](3)

    t.search("anything") shouldBe None
  }

  "delete returns false for non-existent key" in {
    val t = new MemoryBPlusTree[Int, String](3)

    t.delete(1) shouldBe false
  }

  "delete returns false for non-existent key in non-empty tree" in {
    val t = new MemoryBPlusTree[Int, String](3)

    t.insert(1, "one")
    t.insert(3, "three")
    t.insert(5, "five")
    t.delete(2) shouldBe false
    t.delete(0) shouldBe false
    t.delete(6) shouldBe false
    t.wellConstructed shouldBe "true"
    t.keysIterator.toList shouldBe List(1, 3, 5)
  }

  "delete and reinsert same key" in {
    val t = new MemoryBPlusTree[Int, String](3)

    t.insert(1, "one")
    t.insert(2, "two")
    t.insert(3, "three")
    t.delete(2) shouldBe true
    t.search(2) shouldBe None
    t.insert(2, "TWO")
    t.search(2) shouldBe Some("TWO")
    t.wellConstructed shouldBe "true"
    t.keysIterator.toList shouldBe List(1, 2, 3)
  }
end SearchAndDeleteEdgeCaseTests

class ReverseIteratorTests extends AnyFreeSpec with Matchers:

  "reverseIterator returns key-value pairs in descending order" in {
    val t = new MemoryBPlusTree[Int, String](4)

    t.insert(3, "three")
    t.insert(1, "one")
    t.insert(5, "five")
    t.insert(2, "two")
    t.insert(4, "four")

    t.reverseIterator.toList shouldBe List(
      (5, "five"),
      (4, "four"),
      (3, "three"),
      (2, "two"),
      (1, "one"),
    )
  }

  "reverseIterator on empty tree" in {
    val t = new MemoryBPlusTree[Int, String](3)
    t.reverseIterator.isEmpty shouldBe true
  }

  "reverseIterator on single element" in {
    val t = new MemoryBPlusTree[Int, String](3)
    t.insert(42, "answer")
    t.reverseIterator.toList shouldBe List((42, "answer"))
  }
end ReverseIteratorTests

class ValuesIteratorTests extends AnyFreeSpec with Matchers:

  "valuesIterator returns values in key order" in {
    val t = new MemoryBPlusTree[Int, String](4)

    t.insert(3, "three")
    t.insert(1, "one")
    t.insert(5, "five")
    t.insert(2, "two")
    t.insert(4, "four")

    t.valuesIterator.toList shouldBe List("one", "two", "three", "four", "five")
  }

  "valuesIterator on empty tree" in {
    val t = new MemoryBPlusTree[Int, String](3)
    t.valuesIterator.isEmpty shouldBe true
  }

  "boundedValuesIterator" in {
    val t = new MemoryBPlusTree[Int, String](4)

    for i <- 1 to 10 do t.insert(i, s"v$i")

    t.boundedValuesIterator((Bound.Gte, 3), (Bound.Lt, 7)).toList shouldBe List("v3", "v4", "v5", "v6")
  }
end ValuesIteratorTests

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

class MutableSortedMapTests extends AnyFreeSpec with Matchers:

  "default constructor creates empty map" in {
    val m = new MutableSortedMap[String, Int]
    m.isEmpty shouldBe true
    m.size shouldBe 0
  }

  "addOne inserts elements" in {
    val m = new MutableSortedMap[String, Int]
    m.addOne("b" -> 2)
    m.addOne("a" -> 1)
    m.addOne("c" -> 3)

    m.size shouldBe 3
    m("a") shouldBe 1
    m("b") shouldBe 2
    m("c") shouldBe 3
  }

  "+= syntax works" in {
    val m = new MutableSortedMap[String, Int]
    m += ("x" -> 10)
    m += ("y" -> 20)

    m("x") shouldBe 10
    m("y") shouldBe 20
  }

  "subtractOne removes elements" in {
    val m = new MutableSortedMap[String, Int]
    m += ("a" -> 1)
    m += ("b" -> 2)
    m += ("c" -> 3)

    m.subtractOne("b")
    m.size shouldBe 2
    m.get("b") shouldBe None
    m.get("a") shouldBe Some(1)
    m.get("c") shouldBe Some(3)
  }

  "-= syntax works" in {
    val m = new MutableSortedMap[Int, String]
    m += (1 -> "one")
    m += (2 -> "two")
    m -= 1

    m.get(1) shouldBe None
    m.get(2) shouldBe Some("two")
  }

  "get returns None for missing keys" in {
    val m = new MutableSortedMap[String, Int]
    m += ("a" -> 1)

    m.get("a") shouldBe Some(1)
    m.get("b") shouldBe None
  }

  "empty returns a new empty map" in {
    val m = new MutableSortedMap[String, Int]
    m += ("a" -> 1)

    val e = m.empty
    e.isEmpty shouldBe true
    m.size shouldBe 1
  }

  "iterator returns elements in sorted order" in {
    val m = new MutableSortedMap[String, Int]
    m += ("c" -> 3)
    m += ("a" -> 1)
    m += ("b" -> 2)

    m.iterator.toList shouldBe List(("a", 1), ("b", 2), ("c", 3))
  }

  "iteratorFrom returns elements starting from key" in {
    val m = new MutableSortedMap[Int, String]
    for i <- 1 to 10 do m += (i -> s"v$i")

    m.iteratorFrom(5).toList shouldBe (5 to 10).map(i => (i, s"v$i")).toList
  }

  "keysIteratorFrom returns keys starting from key" in {
    val m = new MutableSortedMap[Int, String]
    for i <- 1 to 10 do m += (i -> s"v$i")

    m.keysIteratorFrom(7).toList shouldBe List(7, 8, 9, 10)
  }

  "valuesIteratorFrom returns values starting from key" in {
    val m = new MutableSortedMap[Int, String]
    for i <- 1 to 5 do m += (i -> s"v$i")

    m.valuesIteratorFrom(3).toList shouldBe List("v3", "v4", "v5")
  }

  "rangeImpl with both bounds" in {
    val m = new MutableSortedMap[Int, String]
    for i <- 1 to 10 do m += (i -> s"v$i")

    val r = m.range(3, 7)
    r.iterator.toList shouldBe (3 to 6).map(i => (i, s"v$i")).toList
  }

  "rangeImpl with only lower bound" in {
    val m = new MutableSortedMap[Int, String]
    for i <- 1 to 5 do m += (i -> s"v$i")

    val r = m.rangeFrom(3)
    r.iterator.toList shouldBe (3 to 5).map(i => (i, s"v$i")).toList
  }

  "rangeImpl with only upper bound" in {
    val m = new MutableSortedMap[Int, String]
    for i <- 1 to 5 do m += (i -> s"v$i")

    val r = m.rangeTo(3)
    r.iterator.toList shouldBe (1 to 3).map(i => (i, s"v$i")).toList
  }

  "rangeImpl with no bounds returns all elements" in {
    val m = new MutableSortedMap[Int, String]
    for i <- 1 to 5 do m += (i -> s"v$i")

    val r = m.rangeImpl(None, None)
    r.iterator.toList shouldBe (1 to 5).map(i => (i, s"v$i")).toList
  }

  "overwriting existing key with addOne" in {
    val m = new MutableSortedMap[String, Int]
    m += ("a" -> 1)
    m += ("a" -> 99)

    m("a") shouldBe 99
    m.size shouldBe 1
  }

  "works with Int keys" in {
    val m = new MutableSortedMap[Int, String]
    m += (30 -> "thirty")
    m += (10 -> "ten")
    m += (20 -> "twenty")

    m.keysIterator.toList shouldBe List(10, 20, 30)
  }

  "ordering is correct" in {
    val m = new MutableSortedMap[String, Int]
    m.ordering shouldBe Ordering[String]
  }
end MutableSortedMapTests

class KeysIterableTests extends AnyFreeSpec with Matchers:

  "keys returns an Iterable of keys in order" in {
    val t = new MemoryBPlusTree[Int, String](4)

    t.insert(3, "three")
    t.insert(1, "one")
    t.insert(5, "five")

    t.keys.toList shouldBe List(1, 3, 5)
  }

  "keys is iterable multiple times" in {
    val t = new MemoryBPlusTree[Int, String](4)

    t.insert(2, "two")
    t.insert(1, "one")

    t.keys.toList shouldBe List(1, 2)
    t.keys.toList shouldBe List(1, 2)
  }

  "keys on empty tree" in {
    val t = new MemoryBPlusTree[Int, String](3)
    t.keys.toList shouldBe Nil
  }
end KeysIterableTests
