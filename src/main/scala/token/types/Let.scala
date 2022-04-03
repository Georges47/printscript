package token.types

import org.austral.ingsis.printscript.common.TokenType

case object Let extends TokenType {
  override def getType: String = "Let"

  override def equals(tokenType: TokenType): Boolean = tokenType.getType.equals(getType)
}
