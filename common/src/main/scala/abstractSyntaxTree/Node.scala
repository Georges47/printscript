package abstractSyntaxTree

import org.austral.ingsis.printscript.common.{LexicalRange, TokenType}
import org.austral.ingsis.printscript.parser.Content

object Node {
  def nodeFromContent(content: Content[String]): Node = {
    val value = content.getContent
    val tokenType = content.getToken.getType
    val lexicalRange = content.getToken.getRange
    Node(value, tokenType, Some(lexicalRange))
  }
}

case class Node(value: String, tokenType: TokenType, lexicalRange: Option[LexicalRange] = None)
