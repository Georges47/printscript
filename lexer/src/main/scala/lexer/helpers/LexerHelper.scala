package lexer.helpers

import org.austral.ingsis.printscript.common.LexicalRange

object LexerHelper {
  val helpers = Map(
    "number" -> NumberHelper(),
    "reservedWord" -> ReservedWordHelper(),
    "string" -> StringHelper(),
    "symbol" -> SymbolHelper()
  )
  def newlineRange(range: LexicalRange): LexicalRange = {
    new LexicalRange(
      1,
      range.getStartLine + 1,
      1,
      range.getEndLine + 1
    )
  }
  def rangeAddEndColumn(range: LexicalRange, offset: Int): LexicalRange = {
    new LexicalRange(
      range.getStartCol,
      range.getStartLine,
      range.getEndCol + offset,
      range.getEndLine
    )
  }
}

trait LexerHelper {
  def lex(

                                                                                                      currentNumber: String,
      from: Int,
      to: Int,
      lexicalRange: LexicalRange,
      fileContent: String
  ): LexerHelperResponse
}
