package token.types

import org.austral.ingsis.printscript.common.TokenType

case object Println extends TokenType {
  override def getType: String = "Println"

  override def equals(tokenType: TokenType): Boolean = tokenType.getType.equals(getType)
}
