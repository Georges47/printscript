package lexer.helpers

import lexer.helpers
import org.austral.ingsis.printscript.common.{LexicalRange, Token, TokenType}
import token.types._

case class SymbolHelper() extends LexerHelper {
  override def lex(
      currentNumber: String,
      from: Int,
      to: Int,
      lexicalRange: LexicalRange,
      fileContent: String
  ): HelperResponse = {
    currentNumber match {
      case "("  => helper(OpenParenthesis, from, to, lexicalRange, fileContent)
      case ")"  => helper(ClosedParenthesis, from, to, lexicalRange, fileContent)
      case "-"  => helper(Minus, from, to, lexicalRange, fileContent)
      case "+"  => helper(Plus, from, to, lexicalRange, fileContent)
      case "*"  => helper(Asterisk, from, to, lexicalRange, fileContent)
      case "/"  => helper(FrontSlash, from, to, lexicalRange, fileContent)
      case "&"  => helper(And, from, to, lexicalRange, fileContent)
      case "|"  => helper(Or, from, to, lexicalRange, fileContent)
      case "="  => helper(Assignment, from, to, lexicalRange, fileContent)
      case ":"  => helper(Colon, from, to, lexicalRange, fileContent)
      case ";"  => helper(Semicolon, from, to, lexicalRange, fileContent)
      case "\n" => helper(Newline, from, to, lexicalRange, fileContent)
      case "{"  => helper(OpenBracket, from, to, lexicalRange, fileContent)
      case "}"  => helper(ClosedBracket, from, to, lexicalRange, fileContent)
      case " "  => helper(Whitespace, from, to, lexicalRange, fileContent)
      case "\t" => helper(Tab, from, to, lexicalRange, fileContent)
    }
  }

  private def helper(
      tokenType: TokenType,
      from: Int,
      to: Int,
      lexicalRange: LexicalRange,
      fileContent: String
  ): HelperResponse = {
    val token = new Token(
      tokenType,
      from,
      to + 1,
      lexicalRange
    )
    helpers.HelperResponse(fileContent, token)
  }
}
