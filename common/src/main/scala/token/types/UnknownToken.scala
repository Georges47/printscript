package token.types

import org.austral.ingsis.printscript.common.TokenType

case object UnknownToken extends TokenType {
  override def getType: String = "UnknownToken"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}

