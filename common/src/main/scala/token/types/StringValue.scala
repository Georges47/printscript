package token.types

import org.austral.ingsis.printscript.common.TokenType

case object StringValue extends TokenType {
  override def getType: String = "String"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
