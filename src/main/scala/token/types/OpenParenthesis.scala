package token.types

import org.austral.ingsis.printscript.common.TokenType

case object OpenParenthesis extends TokenType {
  override def getType: String = "OpenParenthesis"

  override def equals(tokenType: TokenType): Boolean = tokenType.getType.equals(getType)
}
