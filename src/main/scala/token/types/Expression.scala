package token.types

import org.austral.ingsis.printscript.common.TokenType

case object Expression extends TokenType {
  override def getType: String = "Expression"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
