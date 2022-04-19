package token.types

import org.austral.ingsis.printscript.common.TokenType

case object Or extends TokenType {
  override def getType: String = "Or"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
