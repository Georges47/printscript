package token.types

import org.austral.ingsis.printscript.common.TokenType

case object Program extends TokenType {
  override def getType: String = "Program"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
