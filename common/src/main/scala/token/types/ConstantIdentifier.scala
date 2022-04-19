package token.types

import org.austral.ingsis.printscript.common.TokenType

case object ConstantIdentifier extends TokenType {
  override def getType: String = "ConstantIdentifier"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
