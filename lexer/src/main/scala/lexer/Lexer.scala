package lexer

import lexer.helpers.LexerHelper
import org.austral.ingsis.printscript.common.{LexicalRange, Token}
import token.types._

import scala.collection.mutable.ListBuffer

object Lexer {
  val keywords = List("let", "const", "println", "readinput", "if")
  val tabSize = 2
  def isDigit(c: Char): Boolean = c.isDigit
  def isIdentifier(c: Char): Boolean = c.toString matches "[_0-9a-zA-Z]"
  def isQuote(c: Char): Boolean = c == '\'' || c == '"'
  def isSymbol(c: Char): Boolean = c.toString matches "[-+*/(){}&|=:; \t\n]"
}

class Lexer(fileContent: String) {
  private var content = ""

  def lex: List[Token] = {
    content = fileContent
    val tokens: ListBuffer[Token] = ListBuffer.empty
    var currentIndex = 0
    var currentLexicalRange = new LexicalRange(1, 1, 1, 1)
    while (content.nonEmpty) {
      val newToken = getToken(currentIndex, currentLexicalRange)
      tokens += newToken
      currentIndex = newToken.getTo
      val newTokenLexicalRange = newToken.getRange
      if (newToken.getType == Newline) {
        currentLexicalRange = LexerHelper.newlineRange(currentLexicalRange)
      } else if (newToken.getType == Tab) {currentLexicalRange = new LexicalRange(







          newTokenLexicalRange.getEndCol + Lexer.tabSize,
          newTokenLexicalRange.getStartLine,
          newTokenLexicalRange.getEndCol + Lexer.tabSize,
          newTokenLexicalRange.getEndLine
        )
      } else {
        currentLexicalRange = new LexicalRange(
          newTokenLexicalRange.getEndCol + 1,
          newTokenLexicalRange.getStartLine,
          newTokenLexicalRange.getEndCol + 1,
          newTokenLexicalRange.getEndLine
        )
      }
    }
    tokens += new Token(
      EndOfFile,
      currentIndex,
      currentIndex,
      currentLexicalRange
    )
    tokens.toList
  }

  private def getToken(
      currentIndex: Int,
      currentLexicalRange: LexicalRange
  ): Token = {
    val currentChar = content.head
    content = content.substring(1)
    var helper = ""

    currentChar match {
      case char if Lexer.isDigit(char)      => helper = "number"
      case char if Lexer.isIdentifier(char) => helper = "reservedWord"
      case char if Lexer.isQuote(char)      => helper = "string"
      case char if Lexer.isSymbol(char)     => helper = "symbol"
      case _ =>
        throw new Exception(
          s"Unknown character $currentChar at line ${currentLexicalRange.getStartLine}, column ${currentLexicalRange.getStartCol}"
        )
    }

    val response = LexerHelper
      .helpers(helper)
      .lex(
        currentChar.toString,
        currentIndex,
        currentIndex,
        currentLexicalRange,
        content
      )

    content = response.fileContent
    response.token
  }
}
