package token.types

import org.austral.ingsis.printscript.common.TokenType

case object Asterisk extends TokenType {
  override def getType: String = "Asterisk"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
