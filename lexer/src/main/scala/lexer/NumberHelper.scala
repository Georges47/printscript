package lexer
import org.austral.ingsis.printscript.common.{LexicalRange, Token}

case class NumberHelper() extends LexerHelper {
  override def lex(currentNumber: String, from: Int, to: Int, lexicalRange: LexicalRange, fileContent: String): HelperResponse = {
    var content = fileContent
    content.head match {
      case char if Lexer.isDigit(char) | char == '.' =>
        content = content.substring(1)
        lex(
          currentNumber + char.toString,
          from,
          to + 1,
          new LexicalRange(
            lexicalRange.getStartCol,
            lexicalRange.getStartLine,
            lexicalRange.getEndCol + 1,
            lexicalRange.getEndLine
          ),
          content
        )
      case _ =>
        HelperResponse(content, new Token(token.types.NumberValue, from, to + 1, lexicalRange))
    }
  }
}
