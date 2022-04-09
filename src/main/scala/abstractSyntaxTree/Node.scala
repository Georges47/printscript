package abstractSyntaxTree

import org.austral.ingsis.printscript.common.TokenType
import org.austral.ingsis.printscript.parser.Content

object Node {
  def nodeFromContent(content: Content[String]): Node = {
    val value = content.getContent
    val tokenType = content.getToken.getType
    Node(value, tokenType)
  }
}

case class Node(value: String, tokenType: TokenType)
