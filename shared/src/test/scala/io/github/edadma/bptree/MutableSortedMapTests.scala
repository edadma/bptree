package io.github.edadma.bptree

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

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
