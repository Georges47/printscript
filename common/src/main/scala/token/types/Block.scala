package token.types

import org.austral.ingsis.printscript.common.TokenType

case object Block extends TokenType {
  override def getType: String = "Block"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
