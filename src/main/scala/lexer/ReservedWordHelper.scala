package lexer

import org.austral.ingsis.printscript.common.{LexicalRange, Token}
import token.types.{Identifier, Let, NumberDataType, Println, StringDataType}

case class ReservedWordHelper() extends LexerHelper {
  override def lex(currentValue: String, from: Int, to: Int, lexicalRange: LexicalRange, fileContent: String): HelperResponse = {
    var content = fileContent
    content.head match {
      case char if currentValue == "let" && char.isWhitespace =>
        HelperResponse(content, new Token(Let, from, to + 1, lexicalRange))
      case char
        if currentValue == "println" && (char.toString matches "[ (]") =>
        HelperResponse(content, new Token(Println, from, to + 1, lexicalRange))
      case char
        if currentValue == "String" && (char.toString matches "[ ;=\n]") =>
        HelperResponse(content, new Token(StringDataType, from, to + 1, lexicalRange))
      case char
        if currentValue == "Number" && (char.toString matches "[ ;=\n]") =>
        HelperResponse(content, new Token(NumberDataType, from, to + 1, lexicalRange))
      case char if char.toString matches "[_0-9a-zA-Z]" =>
        content = content.substring(1)
        lex(
          currentValue + char.toString,
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
        if ( Lexer.keywords.contains(currentValue) ) {
          throw new Exception(s"Attempting to use a reserved word as an identifier name in line ${lexicalRange.getStartLine}, column ${lexicalRange.getStartCol}")
        } else {
          HelperResponse(content, new Token(Identifier, from, to + 1, lexicalRange))
        }
    }
  }
}
