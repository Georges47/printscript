package token.types

import org.austral.ingsis.printscript.common.TokenType

case object Minus extends TokenType {
  override def getType: String = "Minus"

  override def equals(tokenType: TokenType): Boolean = tokenType.getType.equals(getType)
}
