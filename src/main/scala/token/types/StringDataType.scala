package token.types

import org.austral.ingsis.printscript.common.TokenType

case object StringDataType extends TokenType {
  override def getType: String = "StringDataType"

  override def equals(tokenType: TokenType): Boolean = tokenType.getType.equals(getType)
}
