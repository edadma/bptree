package io.github.edadma.bptree

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

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

  "load single element into empty tree" in {
    val t = new MemoryBPlusTree[Int, String](3)

    t.load((42, "answer"))
    t.wellConstructed shouldBe "true"
    t.search(42) shouldBe Some("answer")
    t.keysIterator.toList shouldBe List(42)
  }

  "load single element into non-empty tree" in {
    val t = new MemoryBPlusTree[Int, String](3)

    t.insert(1, "one")
    t.insert(2, "two")
    t.load((3, "three"))
    t.wellConstructed shouldBe "true"
    t.keysIterator.toList shouldBe List(1, 2, 3)
  }
end MultiOrderTests
