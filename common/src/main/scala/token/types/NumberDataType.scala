package token.types

import org.austral.ingsis.printscript.common.TokenType

case object NumberDataType extends TokenType {
  override def getType: String = "NumberDataType"

  override def equals(tokenType: TokenType): Boolean =
    tokenType.getType.equals(getType)
}
