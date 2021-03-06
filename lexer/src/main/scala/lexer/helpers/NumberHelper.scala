package lexer.helpers

import lexer.{Lexer, helpers}
import org.austral.ingsis.printscript.common.{LexicalRange, Token}
import token.types.NumberValue

case class NumberHelper() extends LexerHelper {
  override def lex(
      currentNumber: String,
      from: Int,
      to: Int,
      lexicalRange: LexicalRange,
      fileContent: String
  ): LexerHelperResponse = {
    var content = fileContent
    content.head match {
      case char if Lexer.isDigit(char) | char == '.' =>
        content = content.substring(1)
        lex(
          currentNumber + char.toString,
          from,
          to + 1,
          LexerHelper.rangeAddEndColumn(lexicalRange, 1),
          content
        )
      case _ =>
        helpers.LexerHelperResponse(content, new Token(NumberValue, from, to + 1, lexicalRange))
    }
  }
}
