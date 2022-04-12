package token.types

import org.austral.ingsis.printscript.common.TokenType

case object Plus extends TokenType {
  override def getType: String = "Plus"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
