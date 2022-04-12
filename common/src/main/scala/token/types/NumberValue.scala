package token.types

import org.austral.ingsis.printscript.common.TokenType

case object NumberValue extends TokenType {
  override def getType: String = "Number"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
