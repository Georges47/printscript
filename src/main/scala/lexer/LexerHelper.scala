package lexer

import org.austral.ingsis.printscript.common.{LexicalRange, Token}

trait LexerHelper {
  def lex(currentNumber: String, from: Int, to: Int, lexicalRange: LexicalRange, fileContent: String): HelperResponse
}
