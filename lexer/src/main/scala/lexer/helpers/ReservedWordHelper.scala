package lexer.helpers

import lexer.{Lexer, helpers}
import org.austral.ingsis.printscript.common.{LexicalRange, Token}
import token.types._

case class ReservedWordHelper(/*lastToken: Token, variables: List[String], constants: List[String]*/) extends LexerHelper {
  override def lex(currentValue: String, from: Int, to: Int, lexicalRange: LexicalRange, fileContent: String): HelperResponse = {
    var content = fileContent
    content.head match {
      case char if currentValue == "const" && char.isWhitespace =>
        helpers.HelperResponse(content, new Token(Const, from, to + 1, lexicalRange))
      case char if currentValue == "let" && char.isWhitespace =>
        helpers.HelperResponse(content, new Token(Let, from, to + 1, lexicalRange))
      case char
        if currentValue == "println" && (char.toString matches "[ (]") =>
        helpers.HelperResponse(content, new Token(Println, from, to + 1, lexicalRange))
      case char
        if currentValue == "readInput" && (char.toString matches "[ (]") =>
        helpers.HelperResponse(content, new Token(ReadInput, from, to + 1, lexicalRange))
      case char
        if currentValue == "if" && (char.toString matches "[ (]") =>
        helpers.HelperResponse(content, new Token(If, from, to + 1, lexicalRange))
      case char
        if currentValue == "else" && (char.toString matches "[ {]") =>
        helpers.HelperResponse(content, new Token(Else, from, to + 1, lexicalRange))
      case char if (currentValue == "true" || currentValue == "false") && (char.toString matches "[ &|;)]") =>
        helpers.HelperResponse(content, new Token(BooleanValue, from, to + 1, lexicalRange))
      case char
        if currentValue == "string" && (char.toString matches "[ ;=\n]") =>
        helpers.HelperResponse(content, new Token(StringDataType, from, to + 1, lexicalRange))
      case char
        if currentValue == "number" && (char.toString matches "[ ;=\n]") =>
        helpers.HelperResponse(content, new Token(NumberDataType, from, to + 1, lexicalRange))
      case char
        if currentValue == "boolean" && (char.toString matches "[ ;=\n]") =>
        helpers.HelperResponse(content, new Token(BooleanDataType, from, to + 1, lexicalRange))
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
        if (Lexer.keywords.contains(currentValue)) {
          throw new Exception(s"Attempting to use a reserved word as an identifier name in line ${lexicalRange.getStartLine}, column ${lexicalRange.getStartCol}")
        } else {
          helpers.HelperResponse(content, new Token(Identifier, from, to + 1, lexicalRange))
        }
    }
  }
}
