package token.types

import org.austral.ingsis.printscript.common.TokenType

case object Const extends TokenType {
  override def getType: String = "Const"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
