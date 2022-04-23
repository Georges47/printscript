package token.types

import org.austral.ingsis.printscript.common.TokenType

case object Whitespace extends TokenType {
  override def getType: String = "Whitespace"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
