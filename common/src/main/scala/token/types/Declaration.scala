package token.types

import org.austral.ingsis.printscript.common.TokenType

case object Declaration extends TokenType {
  override def getType: String = "Declaration"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
