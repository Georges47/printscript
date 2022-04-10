package lexer

import org.austral.ingsis.printscript.common.{LexicalRange, Token}
import token.types._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Lexer {
  val keywords = List("let", "println")
  def isDigit(c: Char): Boolean = c.isDigit
  def isIdentifier(c: Char): Boolean = c.toString matches "[_0-9a-zA-Z]"
  def isQuote(c: Char): Boolean = c == '\'' || c == '"'
}

class Lexer(fileContent: String) {
  private var content = ""
  private val helpers = Map("number" -> NumberHelper(),
                            "reservedWord" -> ReservedWordHelper(),
                            "string" -> StringHelper())

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
        currentLexicalRange = new LexicalRange(
          1,
          currentLexicalRange.getStartLine + 1,
          1,
          currentLexicalRange.getEndLine + 1
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

  @tailrec
  private def getToken(
      currentIndex: Int,
      currentLexicalRange: LexicalRange
  ): Token = {
    val currentChar = content.head
    content = content.substring(1)
    currentChar match {
      case char if Lexer.isDigit(char) =>
        val response = helpers("number").lex(
          char.toString,
          currentIndex,
          currentIndex,
          currentLexicalRange,
          content
        )
        content = response.fileContent
        response.token
      case char if Lexer.isIdentifier(char) =>
        val response = helpers("reservedWord").lex(
          char.toString,
          currentIndex,
          currentIndex,
          currentLexicalRange,
          content
        )
        content = response.fileContent
        response.token
      case char if Lexer.isQuote(char) =>
        val response = helpers("string").lex(
          char.toString,
          currentIndex,
          currentIndex + 1,
          currentLexicalRange,
          content
        )
        content = response.fileContent
        response.token
      case '(' =>
        TokenHelper(currentIndex, currentLexicalRange).newToken(OpenParenthesis)
      case ')' =>
        TokenHelper(currentIndex, currentLexicalRange).newToken(ClosedParenthesis)
      case '-' =>
        TokenHelper(currentIndex, currentLexicalRange).newToken(Minus)
      case '+' =>
        TokenHelper(currentIndex, currentLexicalRange).newToken(Plus)
      case '*' =>
        TokenHelper(currentIndex, currentLexicalRange).newToken(Asterisk)
      case '/' =>
        TokenHelper(currentIndex, currentLexicalRange).newToken(FrontSlash)
      case '=' =>
        TokenHelper(currentIndex, currentLexicalRange).newToken(Assignment)
      case ':' =>
        TokenHelper(currentIndex, currentLexicalRange).newToken(Colon)
      case ';' =>
        TokenHelper(currentIndex, currentLexicalRange).newToken(Semicolon)
      case '\n' =>
        TokenHelper(currentIndex, currentLexicalRange).newToken(Newline)
      case ' ' =>
        getToken(
          currentIndex + 1,
          new LexicalRange(
            currentLexicalRange.getStartCol + 1,
            currentLexicalRange.getStartLine,
            currentLexicalRange.getEndCol + 1,
            currentLexicalRange.getEndLine
          )
        )
      case _ => throw new Exception(s"Unknown character at line ${currentLexicalRange.getStartLine}, column ${currentLexicalRange.getStartCol}")
    }
  }
}
