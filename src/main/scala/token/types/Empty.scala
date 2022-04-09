package token.types

import org.austral.ingsis.printscript.common.TokenType

case object Empty extends TokenType {
  override def getType: String = "Empty"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
