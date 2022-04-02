package abstractSyntaxTree

case class AbstractSyntaxTree(value: String = "", nodes: List[AbstractSyntaxTree] = List.empty) {
  override def toString: String = {
    val buffer = new StringBuilder(50)
    print(buffer, "", "")
    buffer.toString
  }

  private def print(buffer: StringBuilder, prefix: String, childrenPrefix: String): Unit = {
    buffer.append(prefix)
    buffer.append(value)
    buffer.append('\n')
    val it = nodes.iterator
    while ( it.hasNext ) {
      val next = it.next
      if (it.hasNext) next.print(buffer, childrenPrefix + "├── ", childrenPrefix + "│   ")
      else next.print(buffer, childrenPrefix + "└── ", childrenPrefix + "    ")
    }
  }
}
