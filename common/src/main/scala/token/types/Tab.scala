package token.types

import org.austral.ingsis.printscript.common.TokenType

case object Tab extends TokenType {
  override def getType: String = "Tab"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
