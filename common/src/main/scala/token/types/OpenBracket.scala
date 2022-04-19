package token.types

import org.austral.ingsis.printscript.common.TokenType

case object OpenBracket extends TokenType {
  override def getType: String = "OpenBracket"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
