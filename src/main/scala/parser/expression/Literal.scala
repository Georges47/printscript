package parser.expression
import org.austral.ingsis.printscript.common.TokenType
import org.austral.ingsis.printscript.parser.Content

case class Literal(content: Content[String]) extends Expression {
  override def value: String = content.getContent

  override def expressionType: TokenType = content.getToken.getType
}
