package token.types

import org.austral.ingsis.printscript.common.TokenType

case object VariableIdentifier extends TokenType {
  override def getType: String = "VariableIdentifier"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
