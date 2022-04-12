package token.types

import org.austral.ingsis.printscript.common.TokenType

case object Newline extends TokenType {
  override def getType: String = "Newline"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
