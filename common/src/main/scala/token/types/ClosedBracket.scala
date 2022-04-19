package token.types

import org.austral.ingsis.printscript.common.TokenType

case object ClosedBracket extends TokenType {
  override def getType: String = "ClosedBracket"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
