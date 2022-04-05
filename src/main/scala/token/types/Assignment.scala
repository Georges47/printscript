package token.types

import org.austral.ingsis.printscript.common.TokenType

case object Assignment extends TokenType {
  override def getType: String = "Assignment"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
