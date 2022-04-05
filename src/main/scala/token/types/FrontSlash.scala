package token.types

import org.austral.ingsis.printscript.common.TokenType

case object FrontSlash extends TokenType {
  override def getType: String = "FrontSlash"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
