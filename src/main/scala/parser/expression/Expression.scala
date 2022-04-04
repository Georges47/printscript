package parser.expression

import org.austral.ingsis.printscript.common.TokenType
import org.austral.ingsis.printscript.parser.Content

trait Expression {
  def content: Content[String]
  def value: String
  def expressionType: TokenType
}
