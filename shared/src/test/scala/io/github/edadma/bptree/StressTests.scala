package io.github.edadma.bptree

import scala.util.Random

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

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
