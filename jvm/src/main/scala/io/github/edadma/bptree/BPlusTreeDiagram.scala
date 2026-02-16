package io.github.edadma.bptree

import scala.sys.process.*
import java.io.PrintWriter

extension [K: Ordering, V](tree: BPlusTree[K, V])
  /**
   * Creates a PNG image file called `name` (with `.png` added) which visually represents the structure and contents of the tree, only showing the keys.
   */
  def diagram(name: String): Unit =
    val file = new PrintWriter(name + ".dot")
    file.println(tree.dotString)
    file.close()
    s"dot -Tsvg $name.dot -o $name.svg".!
    s"convert $name.svg $name.png".!
