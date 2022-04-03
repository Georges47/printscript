package token.types

import org.austral.ingsis.printscript.common.TokenType

case object Semicolon extends TokenType {
  override def getType: String = "Semicolon"

  override def equals(tokenType: TokenType): Boolean = tokenType.getType.equals(getType)
}
