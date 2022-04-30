package lexer.helpers

import lexer.{Lexer, helpers}
import org.austral.ingsis.printscript.common.{LexicalRange, Token, TokenType}
import token.types._

import scala.annotation.tailrec

private case class HelperResponse(tokenType: TokenType, to: Int, range: LexicalRange)

case class ReservedWordHelper() extends LexerHelper {
  var content: String = ""

  override def lex(
      currentValue: String,
      from: Int,
      to: Int,
      lexicalRange: LexicalRange,
      fileContent: String
  ): LexerHelperResponse = {
    content = fileContent
    val helperResponse = helper(currentValue, from, to, lexicalRange)
    helpers.LexerHelperResponse(
      content,
      new Token(
        helperResponse.tokenType,
        from,
        helperResponse.to + 1,
        helperResponse.range
      )
    )
  }

  @tailrec
  private def helper(
      currentString: String,
      from: Int,
      to: Int,
      range: LexicalRange
  ): HelperResponse = {
    content.head match {
      case char if currentString == "const" && char.isWhitespace => HelperResponse(Const, to, range)
      case char if currentString == "let" && char.isWhitespace   => HelperResponse(Let, to, range)
      case char if currentString == "println" && (char.toString matches "[ (]") =>
        HelperResponse(Println, to, range)
      case char if currentString == "readInput" && (char.toString matches "[ (]") =>
        HelperResponse(ReadInput, to, range)
      case char if currentString == "if" && (char.toString matches "[ (]") =>
        HelperResponse(If, to, range)
      case char if currentString == "else" && (char.toString matches "[ {]") =>
        HelperResponse(Else, to, range)
      case char
          if (currentString == "true" || currentString == "false") && (char.toString matches "[ &|;)]") =>
        HelperResponse(BooleanValue, to, range)
      case char if currentString == "string" && (char.toString matches "[ ;=\n]") =>
        HelperResponse(StringDataType, to, range)
      case char if currentString == "number" && (char.toString matches "[ ;=\n]") =>
        HelperResponse(NumberDataType, to, range)
      case char if currentString == "boolean" && (char.toString matches "[ ;=\n]") =>
        HelperResponse(BooleanDataType, to, range)
      case char if char.toString matches "[_0-9a-zA-Z]" =>
        content = content.substring(1)
        helper(
          currentString + char.toString,
          from,
          to + 1,
          LexerHelper.rangeAddEndColumn(range, 1)
        )
      case _ =>
        if (Lexer.keywords.contains(currentString)) {
          throw new Exception(
            s"Attempting to use a reserved word as an identifier name in line ${range.getStartLine}, column ${range.getStartCol}"
          )
        } else {
          HelperResponse(Identifier, to, range)
        }
    }
  }
}
