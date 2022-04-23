package lexer.helpers

import org.austral.ingsis.printscript.common.LexicalRange

object LexerHelper {
  val helpers = Map(
    "number" -> NumberHelper(),
    "reservedWord" -> ReservedWordHelper(),
    "string" -> StringHelper(),
    "symbol" -> SymbolHelper()
  )
}

trait LexerHelper {
  def lex(currentNumber: String, from: Int, to: Int, lexicalRange: LexicalRange, fileContent: String): HelperResponse
}
