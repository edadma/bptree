# bptree

A general-purpose B+ tree library for Scala 3, cross-compiled to JVM, Scala.js, and Scala Native.

## Features

- **Cross-platform** -- runs on JVM, Scala.js, and Scala Native
- **In-memory and on-disk** -- `MemoryBPlusTree` for in-memory use; `FileBPlusTree` (JVM-only) for persistent storage
- **Sorted iteration** -- forward, reverse, and bounded range iterators with `Gt`, `Gte`, `Lt`, `Lte` bounds
- **Bulk loading** -- efficient `load` method for inserting pre-sorted data
- **Scala collections integration** -- `MutableSortedMap` wraps a B+ tree as a `scala.collection.mutable.SortedMap`
- **Configurable order** -- branching factor of 3 or higher
- **Extensible** -- `BPlusTree` is an abstract class; implement the storage primitives to back the tree with any storage engine

## Installation

Add to your `build.sbt`:

```scala
libraryDependencies += "io.github.edadma" %%% "bptree" % "0.0.1"
```

For JVM-only:

```scala
libraryDependencies += "io.github.edadma" %% "bptree" % "0.0.1"
```

## Usage

### Basic operations

```scala
import io.github.edadma.bptree.*

val tree = new MemoryBPlusTree[String, Int](order = 4)

// Insert
tree.insert("alice", 1)
tree.insert("bob", 2)
tree.insert("carol", 3)

// Lookup
tree.search("bob")    // Some(2)
tree.search("dave")   // None

// Delete
tree.delete("bob")    // true  (key existed)
tree.delete("dave")   // false (key not found)

// Min/max
tree.min     // Some(("alice", 1))
tree.max     // Some(("carol", 3))
tree.minKey  // Some("alice")
tree.maxKey  // Some("carol")
```

### Iteration

```scala
// All entries in key order
tree.iterator.toList       // List(("alice", 1), ("carol", 3))
tree.keysIterator.toList   // List("alice", "carol")
tree.valuesIterator.toList // List(1, 3)

// Reverse order
tree.reverseIterator.toList // List(("carol", 3), ("alice", 1))

// Bounded range queries
tree.boundedIterator((Bound.Gte, "b"), (Bound.Lt, "d")).toList
// entries where key >= "b" and key < "d"

tree.reverseBoundedKeysIterator((Bound.Gte, "a"), (Bound.Lte, "c")).toList
// keys in descending order where key >= "a" and key <= "c"
```

### Neighbor queries

```scala
val tree = new MemoryBPlusTree[Int, String](order = 4)
for i <- List(10, 20, 30, 40, 50) do tree.insert(i, s"v$i")

tree.leastGreaterThanOrEqual(25)  // Some((30, "v30"))
tree.leastGreaterThan(30)         // Some((40, "v40"))
tree.greatestLessThanOrEqual(25)  // Some((20, "v20"))
tree.greatestLessThan(20)         // Some((10, "v10"))
```

### Bulk loading

```scala
val tree = new MemoryBPlusTree[Int, String](order = 4)

// load sorts the data and inserts efficiently
tree.load((3, "c"), (1, "a"), (2, "b"))
tree.keysIterator.toList  // List(1, 2, 3)
```

### As a Scala SortedMap

```scala
val m = new MutableSortedMap[String, Int]

m += ("banana" -> 2)
m += ("apple" -> 1)
m += ("cherry" -> 3)

m.iterator.toList     // List(("apple", 1), ("banana", 2), ("cherry", 3))
m.range("b", "c").toList  // List(("banana", 2))
m("apple")            // 1
m -= "apple"
m.get("apple")        // None
```

### File-backed tree (JVM only)

```scala
import io.github.edadma.bptree.*

// Create a new file-backed tree
val tree = new FileBPlusTree[String, String]("data.db", order = 10, create = true)

tree.insert("key1", "value1")
tree.search("key1")  // Some("value1")

tree.close()

// Reopen an existing tree
val tree2 = new FileBPlusTree[String, String]("data.db", order = 10)
tree2.search("key1")  // Some("value1")
tree2.close()
```

## API Overview

### BPlusTree\[K, V\]

Abstract base class. Key type `K` requires an `Ordering` instance.

| Method | Description |
|--------|-------------|
| `insert(key, value)` | Insert or update. Returns `true` if key existed |
| `insertIfNotFound(key, value)` | Insert only if absent. Returns `true` if key existed |
| `delete(key)` | Remove a key. Returns `true` if found |
| `search(key)` | Lookup. Returns `Option[V]` |
| `load(kvs*)` | Bulk-insert sorted or unsorted key-value pairs |
| `isEmpty` | Whether the tree is empty |
| `min` / `max` | First/last entry as `Option[(K, V)]` |
| `minKey` / `maxKey` | First/last key as `Option[K]` |
| `iterator` / `reverseIterator` | Full traversal iterators |
| `keysIterator` / `valuesIterator` | Key-only or value-only iterators |
| `boundedIterator(bounds*)` | Range query with `Bound.Gt`, `Gte`, `Lt`, `Lte` |
| `reverseBoundedIterator(bounds*)` | Reverse range query |
| `leastGreaterThan(key)` | Next entry strictly after key |
| `leastGreaterThanOrEqual(key)` | Next entry at or after key |
| `greatestLessThan(key)` | Previous entry strictly before key |
| `greatestLessThanOrEqual(key)` | Previous entry at or before key |
| `keys` | Iterable of all keys in order |
| `wellConstructed` | Structural integrity check (returns `"true"` or error description) |

### Implementations

- **`MemoryBPlusTree[K, V](order)`** -- in-memory, cross-platform
- **`FileBPlusTree[K, V](filename, order, create?)`** -- file-backed, JVM-only
- **`MutableSortedMap[K, V]`** -- wraps a B+ tree as `scala.collection.mutable.SortedMap`

## License

ISC
