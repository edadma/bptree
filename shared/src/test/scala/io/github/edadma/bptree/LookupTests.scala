package io.github.edadma.bptree

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

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
