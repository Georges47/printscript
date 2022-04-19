package token.types

import org.austral.ingsis.printscript.common.TokenType

case object BooleanDataType extends TokenType {
  override def getType: String = "BooleanDataType"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
