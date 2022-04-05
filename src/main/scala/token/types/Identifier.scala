package token.types

import org.austral.ingsis.printscript.common.TokenType

case object Identifier extends TokenType {
  override def getType: String = "Identifier"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
