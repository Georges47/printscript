package abstractSyntaxTree

case class AbstractSyntaxTree(
    root: Node,
    nodes: List[AbstractSyntaxTree] = List.empty
) {
  override def toString: String = {
    val buffer = new StringBuilder(50)
    print(buffer, "", "")
    buffer.toString
  }

  private def print(
      buffer: StringBuilder,
      prefix: String,
      childrenPrefix: String
  ): Unit = {
    buffer.append(prefix)
    buffer.append(root.value)
    buffer.append('\n')
    val it = nodes.iterator
    while (it.hasNext) {
      val next = it.next
      if (it.hasNext)
        next.print(buffer, childrenPrefix + "├── ", childrenPrefix + "│   ")
      else
        next.print(buffer, childrenPrefix + "└── ", childrenPrefix + "    ")
    }
  }

//  override def equals(ast: AbstractSyntaxTree): Boolean = {
//    if (root == ast.root) {
//
//    } else {
//      false
//    }
//  }
}
