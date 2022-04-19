package token.types

import org.austral.ingsis.printscript.common.TokenType

case object Else extends TokenType {
  override def getType: String = "Else"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}

