package io.github.edadma.bptree

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

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
