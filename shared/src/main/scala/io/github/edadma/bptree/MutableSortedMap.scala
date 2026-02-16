package io.github.edadma.bptree

import scala.collection.mutable

object MutableSortedMap:
  private val DEFAULT_ORDER = 10

class MutableSortedMap[K, V](btree: BPlusTree[K, V])(using val ordering: Ordering[K]) extends mutable.SortedMap[K, V]:
  def this()(using Ordering[K]) = this(new MemoryBPlusTree[K, V](MutableSortedMap.DEFAULT_ORDER))

  def addOne(kv: (K, V)): this.type =
    kv match { case (k, v) => btree.insert(k, v) }
    this

  def subtractOne(key: K): this.type =
    btree.delete(key)
    this

  override def empty: MutableSortedMap[K, V] = new MutableSortedMap[K, V]

  def get(key: K): Option[V] = btree.search(key)

  def iterator: Iterator[(K, V)] = btree.iterator

  override def iteratorFrom(start: K): Iterator[(K, V)] = btree.boundedIterator((Bound.Gte, start))

  override def keysIteratorFrom(start: K): Iterator[K] = btree.boundedKeysIterator((Bound.Gte, start))

  override def valuesIteratorFrom(start: K): Iterator[V] = btree.boundedValuesIterator((Bound.Gte, start))

  def rangeImpl(from: Option[K], until: Option[K]): MutableSortedMap[K, V] =
    val bounds =
      (from, until) match
        case (None, None)       => Nil
        case (None, Some(u))    => List((Bound.Lt, u))
        case (Some(l), None)    => List((Bound.Gte, l))
        case (Some(l), Some(u)) => List((Bound.Gte, l), (Bound.Lt, u))

    new MutableSortedMap[K, V](btree):
      override def iterator: Iterator[(K, V)] =
        if bounds.isEmpty then btree.iterator else btree.boundedIterator(bounds*)
end MutableSortedMap
