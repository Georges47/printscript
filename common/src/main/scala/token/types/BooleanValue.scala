package token.types

import org.austral.ingsis.printscript.common.TokenType

case object BooleanValue extends TokenType {
  override def getType: String = "BooleanValue"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
