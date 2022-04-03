package token.types

import org.austral.ingsis.printscript.common.TokenType

case object Colon extends TokenType {
  override def getType: String = "Colon"

  override def equals(tokenType: TokenType): Boolean = tokenType.getType.equals(getType)
}
