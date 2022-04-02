package lexer

import token.{AssignmentOperator, Colon, DoubleToken, EndOfFile, IdentifierToken, IntegerToken, KeywordToken, LexicalRange, NewLine, OperatorToken, StatementDelimiter, StringToken, Token}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Lexer {
  val keywords = List("let", "println")
  def isDigit(c: Char): Boolean = c.isDigit
  def isIdentifier(c: Char): Boolean = c.toString matches "[_0-9a-zA-Z]"
  def isOperator(c: Char): Boolean = c.toString matches "[-+*/]"
}

class Lexer/*(val iterator: Iterator[Char])*/ {
  def getTokens(iterator: Iterator[Char]): List[Token] = {
    val tokens: ListBuffer[Token] = ListBuffer.empty
    var currentIndex = 1
    var currentLexicalRange = LexicalRange(1, 1, 1, 1)
    while (iterator.hasNext) {
      val newToken = getToken(currentIndex, currentLexicalRange, iterator)
      tokens += newToken
      currentIndex = newToken.to + 1
      // subo en columna si hay \n
      // subo en caracter +1 aca
      val newTokenLexicalRange = newToken.lexicalRange
      // new LexicalRange(currentLexicalRange.startColumn, currentLexicalRange.endColumn, currentLexicalRange.startLine + 1, currentLexicalRange.endLine + 1)
      newToken match {
        case NewLine(_, _, _) => currentLexicalRange = LexicalRange(1, 1, currentLexicalRange.startLine + 1, currentLexicalRange.endLine + 1)
        case _ => currentLexicalRange = LexicalRange(newTokenLexicalRange.endColumn + 1, newTokenLexicalRange.endColumn + 1, newTokenLexicalRange.startLine, newTokenLexicalRange.endLine)
      }
    }
    tokens += EndOfFile(from = currentIndex, to = currentIndex, currentLexicalRange)
    tokens.toList
  }

  // LexicalRange public constructor(startCol: kotlin.Int, startLine: kotlin.Int, endCol: kotlin.Int, endLine: kotlin.Int)

  @tailrec
  private def getToken(currentIndex: Int, currentLexicalRange: LexicalRange, iterator: Iterator[Char]): Token = {
    popCharacter(iterator) match {
//      case None => EndOfFile(from = currentIndex-1, to = currentIndex-1, currentLexicalRange)
      case Some(char) => char match {
        case char if Lexer.isDigit(char) => processNumber(char.toString, currentIndex, currentIndex, currentLexicalRange, iterator)
        case char if Lexer.isIdentifier(char) => processIdentifier(char.toString, currentIndex, currentIndex, currentLexicalRange, iterator)
        case char if Lexer.isOperator(char) => OperatorToken(char.toString, from = currentIndex, to = currentIndex, currentLexicalRange)
        case '\'' | '"' => processString(char.toString, currentIndex, currentIndex, currentLexicalRange, iterator)
        case '=' => AssignmentOperator(from = currentIndex, to = currentIndex, currentLexicalRange)
        case ':' => Colon(from = currentIndex, to = currentIndex, currentLexicalRange)
        case ';' => StatementDelimiter(from = currentIndex, to = currentIndex, currentLexicalRange)
        case '\n' => NewLine(from = currentIndex, to = currentIndex, currentLexicalRange)
        case ' ' => getToken(
          currentIndex+1,
          LexicalRange(currentLexicalRange.startColumn + 1, currentLexicalRange.endColumn + 1, currentLexicalRange.startLine, currentLexicalRange.endLine),
          iterator)
      }
    }
  }

  private def readCharacter(iterator: Iterator[Char]): Option[Char] = {
    if (iterator.hasNext) Some(iterator.buffered.head) else None
  }

  private def popCharacter(iterator: Iterator[Char]): Option[Char] = {
    if (iterator.hasNext) Some(iterator.next) else None
  }

  @tailrec
  private def processNumber(currentNumber: String, from: Int, to: Int, lexicalRange: LexicalRange, iterator: Iterator[Char]): Token = {
    readCharacter(iterator) match {
      //case None => if (currentNumber.contains('.')) DoubleToken(currentNumber, from, to) else IntegerToken(currentNumber, from, to)
      case None => throw new Exception(s"Error at $to")
      case Some(char) => char match {
        case char if Lexer.isDigit(char) | char == '.' => iterator.next; processNumber(
          currentNumber + char.toString,
          from,
          to + 1,
          LexicalRange(lexicalRange.startColumn, lexicalRange.endColumn + 1, lexicalRange.startLine, lexicalRange.endLine),
          iterator)
        case _  => if (currentNumber.contains('.')) DoubleToken(currentNumber, from, to, lexicalRange) else IntegerToken(currentNumber, from, to, lexicalRange)
      }
    }
  }

  @tailrec
  private def processString(currentString: String, from: Int, to: Int, lexicalRange: LexicalRange, iterator: Iterator[Char]): Token = {
    val initialQuote = currentString.head
    popCharacter(iterator) match {
      case None => throw new Exception("Malformed string, no closing quote")
      case Some(char) => char match {
        case char if char == initialQuote => StringToken(
          currentString.substring(1),
          from,
          to + 1,
          LexicalRange(lexicalRange.startColumn, lexicalRange.endColumn + 1, lexicalRange.startLine, lexicalRange.endLine)
        )
        case _ => processString(
          currentString + char,
          from,
          to + 1,
          LexicalRange(lexicalRange.startColumn, lexicalRange.endColumn + 1, lexicalRange.startLine, lexicalRange.endLine),
          iterator)
      }
    }
  }

  @tailrec
  private def processIdentifier(currentValue: String, from: Int, to: Int, lexicalRange: LexicalRange, iterator: Iterator[Char]): Token = {
    readCharacter(iterator) match {
      case None => IdentifierToken(currentValue, from, to, lexicalRange)
      case Some(char) => char match {
        case char if char.toString matches "[_0-9a-zA-Z]" => iterator.next; processIdentifier(
          currentValue + char.toString,
          from,
          to + 1,
          LexicalRange(lexicalRange.startColumn, lexicalRange.endColumn + 1, lexicalRange.startLine, lexicalRange.endLine),
          iterator)
        case char if Lexer.keywords.contains(currentValue) && char.isWhitespace => KeywordToken(currentValue, from, to, lexicalRange)
        case _ => IdentifierToken(currentValue, from, to, lexicalRange)
      }
    }
  }
}
