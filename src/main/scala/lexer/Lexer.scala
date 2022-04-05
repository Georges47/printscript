package lexer

import lexer.Lexer.isQuote
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

class Lexer {
  private var content = ""

  def getTokens(fileContent: String): List[Token] = {
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
//      content: String
  ): Token = {
    println("Content " + content)
    content.head match {
//      case None =>
//        new Token(
//          EndOfFile,
//          currentIndex - 1,
//          currentIndex - 1,
//          currentLexicalRange
//        )
//      case Some(char) =>
//        char match {
      case char if Lexer.isDigit(char) =>
        content = content.substring(1)
        processNumber(
          char.toString,
          currentIndex,
          currentIndex,
          currentLexicalRange
//              content.substring(1)
        )
      case char if Lexer.isIdentifier(char) =>
        content = content.substring(1)
        processIdentifier(
          char.toString,
          currentIndex,
          currentIndex,
          currentLexicalRange
//              content.substring(1)
        )
      case '(' =>
        content = content.substring(1)
        new Token(
          OpenParenthesis,
          currentIndex,
          currentIndex + 1,
          currentLexicalRange
        )
      case ')' =>
        content = content.substring(1)
        new Token(
          ClosedParenthesis,
          currentIndex,
          currentIndex + 1,
          currentLexicalRange
        )
      case '-' =>
        content = content.substring(1)
        new Token(
          Minus,
          currentIndex,
          currentIndex + 1,
          currentLexicalRange
        )
      case '+' =>
        content = content.substring(1)
        new Token(Plus, currentIndex, currentIndex + 1, currentLexicalRange)
      case '*' =>
        content = content.substring(1)
        new Token(
          Asterisk,
          currentIndex,
          currentIndex + 1,
          currentLexicalRange
        )
      case '/' =>
        content = content.substring(1)
        new Token(
          FrontSlash,
          currentIndex,
          currentIndex + 1,
          currentLexicalRange
        )
      case char if Lexer.isQuote(char) =>
        content = content.substring(1)
        processString(
          char.toString,
          currentIndex,
          currentIndex + 1,
          currentLexicalRange
//              content.substring(1)
        )
      case '=' =>
        content = content.substring(1)
        new Token(
          Assignment,
          currentIndex,
          currentIndex + 1,
          currentLexicalRange
        )
      case ':' =>
        content = content.substring(1)
        new Token(
          Colon,
          currentIndex,
          currentIndex + 1,
          currentLexicalRange
        )
      case ';' =>
        content = content.substring(1)
        new Token(
          Semicolon,
          currentIndex,
          currentIndex + 1,
          currentLexicalRange
        )
      case '\n' =>
        content = content.substring(1)
        new Token(
          Newline,
          currentIndex,
          currentIndex + 1,
          currentLexicalRange
        )
      case ' ' =>
        content = content.substring(1)
        getToken(
          currentIndex + 1,
          new LexicalRange(
            currentLexicalRange.getStartCol + 1,
            currentLexicalRange.getStartLine,
            currentLexicalRange.getEndCol + 1,
            currentLexicalRange.getEndLine
          )
        )
//        }
    }
  }

  private def readCharacter(iterator: Iterator[Char]): Option[Char] = {
    if (iterator.hasNext) Some(iterator.buffered.head) else None
  }

//  private def popCharacter(iterator: Iterator[Char]): Option[Char] = {
//    if (iterator.hasNext) Some(iterator.next) else None
//  }

  @tailrec
  private def processNumber(
      currentNumber: String,
      from: Int,
      to: Int,
      lexicalRange: LexicalRange
//      content: String
  ): Token = {
    content.head match {
//      case None => throw new Exception(s"Error at $to")
//      case Some(char) =>
//        char match {
      case char if Lexer.isDigit(char) | char == '.' =>
//            iterator.next
        content = content.substring(1)
        processNumber(
          currentNumber + char.toString,
          from,
          to + 1,
          new LexicalRange(
            lexicalRange.getStartCol,
            lexicalRange.getStartLine,
            lexicalRange.getEndCol + 1,
            lexicalRange.getEndLine
          )
//              content.substring(1)
        )
      case _ =>
        new Token(token.types.NumberValue, from, to + 1, lexicalRange)
//        }
    }
  }

  @tailrec
  private def processString(
      currentString: String,
      from: Int,
      to: Int,
      lexicalRange: LexicalRange
//      content: String
  ): Token = {
    val initialQuote = currentString.head
    content.head match {
//      case None => throw new Exception("Malformed string, no closing quote")
//      case Some(char) =>
//        char match {
      case char if char == initialQuote =>
        content = content.substring(1)
        new Token(
          token.types.StringValue,
          from,
          to + 1,
          new LexicalRange(
            lexicalRange.getStartCol,
            lexicalRange.getStartLine,
            lexicalRange.getEndCol + 1,
            lexicalRange.getEndLine
          )
        )
      case _ =>
        println("current string: " + currentString)
        val newChar = content.head
        content = content.substring(1)
        processString(
          currentString + newChar,
          from,
          to + 1,
          new LexicalRange(
            lexicalRange.getStartCol,
            lexicalRange.getStartLine,
            lexicalRange.getEndCol + 1,
            lexicalRange.getEndLine
          )
//              content.substring(1)
        )
//        }
    }
  }

  @tailrec
  private def processIdentifier(
      currentValue: String,
      from: Int,
      to: Int,
      lexicalRange: LexicalRange
//      content: String
  ): Token = {
    println("--- --- ---")
    println("currentvalue: " + currentValue)
    println("char: " + content.head)
    println("matches: " + (content.head.toString matches "[_0-9a-zA-Z]"))
    println(content.head.isLetter || content.head.isDigit)
    content.head match {
//      case None =>
//        currentValue match { // no se que tan necesaria es esta parte
//          case "String" => new Token(StringDataType, from, to + 1, lexicalRange)
//          case "Number" => new Token(NumberDataType, from, to + 1, lexicalRange)
//          case _        => new Token(Identifier, from, to + 1, lexicalRange)
//        }
//      case Some(char) =>
//        char match {
      case char if char.toString matches "[_0-9a-zA-Z]" =>
        println("1")
//            iterator.next
        content = content.substring(1)
        processIdentifier(
          currentValue + char.toString,
          from,
          to + 1,
          new LexicalRange(
            lexicalRange.getStartCol,
            lexicalRange.getStartLine,
            lexicalRange.getEndCol + 1,
            lexicalRange.getEndLine
          )
//              content.substring(1)
        )
      case char if currentValue == "let" && char.isWhitespace =>
        new Token(Let, from, to + 1, lexicalRange)
      case char
          if currentValue == "println" && (char.toString matches "[ (]") =>
        new Token(Println, from, to + 1, lexicalRange)
      case char
          if currentValue == "String" && (char.toString matches "[ ;=\n]") =>
        new Token(StringDataType, from, to + 1, lexicalRange)
      case char
          if currentValue == "Number" && (char.toString matches "[ ;=\n]") =>
        new Token(NumberDataType, from, to + 1, lexicalRange)
      case _ => println("6"); new Token(Identifier, from, to + 1, lexicalRange)
    }
//    }
  }
}
