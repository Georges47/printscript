package abstractSyntaxTree

import org.austral.ingsis.printscript.parser.Content

case class AbstractSyntaxTree(
    content: Content[String],
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
    if (content.getContent.nonEmpty) {
      buffer.append(prefix)
      buffer.append(content.getContent)
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
  }
}
