package token.types

import org.austral.ingsis.printscript.common.TokenType

case object DeclarationAndAssignment extends TokenType {
  override def getType: String = "DeclarationAndAssignment"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
