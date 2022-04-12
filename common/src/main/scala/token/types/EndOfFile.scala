package token.types

import org.austral.ingsis.printscript.common.TokenType

case object EndOfFile extends TokenType {
  override def getType: String = "EndOfFile"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
