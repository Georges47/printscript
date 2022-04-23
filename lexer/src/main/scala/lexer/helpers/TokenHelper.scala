package lexer.helpers

import org.austral.ingsis.printscript.common.{LexicalRange, Token, TokenType}

case class TokenHelper(currentIndex: Int, currentLexicalRange: LexicalRange) {
  def newToken(tokenType: TokenType): Token = {
    new Token(
      tokenType,
      currentIndex,
      currentIndex + 1,
      currentLexicalRange
    )
  }
}
