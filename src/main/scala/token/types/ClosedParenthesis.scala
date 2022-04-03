package token.types

import org.austral.ingsis.printscript.common.TokenType

case object ClosedParenthesis extends TokenType {
  override def getType: String = "ClosedParenthesis"

  override def equals(tokenType: TokenType): Boolean = tokenType.getType.equals(getType)
}
