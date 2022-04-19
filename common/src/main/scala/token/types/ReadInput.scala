package token.types

import org.austral.ingsis.printscript.common.TokenType

case object ReadInput extends TokenType {
  override def getType: String = "ReadInput"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
