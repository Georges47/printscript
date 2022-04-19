package token.types

import org.austral.ingsis.printscript.common.TokenType

case object And extends TokenType {
  override def getType: String = "And"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
