package token.types

import org.austral.ingsis.printscript.common.TokenType

case object If extends TokenType {
  override def getType: String = "If"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
