package lexer

import token.{AssignmentOperator, Colon, DoubleToken, EndOfFile, IdentifierToken, IntegerToken, KeywordToken, OperatorToken, StatementDelimiter, StringToken, Token}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Lexer {
  val keywords = List("let", "println")
  def isDigit(c: Char): Boolean = c.isDigit
  def isIdentifier(c: Char): Boolean = c.toString matches "[_0-9a-zA-Z]"
  def isOperator(c: Char): Boolean = c.toString matches "[-+*/]"
}

class Lexer {
  def getTokens(iterator: Iterator[Char]): List[Token] = {
    val tokens: ListBuffer[Token] = ListBuffer.empty
    var currentIndex = 1
    while (iterator.hasNext) {
      tokens += getToken(currentIndex, iterator)
      currentIndex = tokens.last.to + 1
    }
    tokens.toList
  }

  @tailrec
  private def getToken(currentIndex: Int, iterator: Iterator[Char]): Token = {
    readCharacter(iterator) match {
      case None => EndOfFile(from = currentIndex-1, to = currentIndex-1)
      case Some(char) => char match {
        case char if Lexer.isDigit(char) => processNumber(char.toString, currentIndex, currentIndex, iterator, false)
        case char if Lexer.isIdentifier(char) => processIdentifier(char.toString, currentIndex, currentIndex, iterator)
        case char if Lexer.isOperator(char) => OperatorToken(char.toString, from = currentIndex, to = currentIndex)
        case '\'' | '"' => processString(char.toString, currentIndex, currentIndex, iterator)
        case '=' => AssignmentOperator(from = currentIndex, to = currentIndex)
        case ':' => Colon(from = currentIndex, to = currentIndex)
        case ';' => StatementDelimiter(from = currentIndex, to = currentIndex)
        case _ => getToken(currentIndex+1, iterator)
      }
    }
  }

  private def readCharacter(iterator: Iterator[Char]): Option[Char] = {
    if (iterator.hasNext) Some(iterator.next) else None
  }

  @tailrec
  private def processNumber(currentNumber: String, from: Int, to: Int, iterator: Iterator[Char],  hasDecimalPoint : Boolean): Token = {
    var hasDecimal = hasDecimalPoint;
    readCharacter(iterator) match {
      case None => if (currentNumber.contains('.')) DoubleToken(currentNumber, from, to) else IntegerToken(currentNumber, from, to)
      case Some(char) => char match {
        case char if Lexer.isDigit(char) => processNumber(currentNumber + char.toString, from, to + 1, iterator, hasDecimal)
        case '.' => if(hasDecimal) throw new Exception("Cant have more than 1 decimal points")
          else {
            hasDecimal = !hasDecimal
            var decimalPoint = char.toString
            val nextChar = iterator.next
            nextChar match {
            case ' ' | ';' => throw new Exception("Must have a number after a decimal point")
            case _ => processNumber(currentNumber + '.' + nextChar.toString, from, to + 1, iterator, hasDecimal)
          }
        }
        case _  => if (currentNumber.contains('.')) DoubleToken(currentNumber, from, to) else IntegerToken(currentNumber, from, to)
      }
    }
  }

  @tailrec
  private def processString(currentString: String, from: Int, to: Int, iterator: Iterator[Char]): Token = {
    val initialQuote = currentString.head
    readCharacter(iterator) match {
      case None => throw new Exception("Malformed string, no closing quote")
      case Some(char) => char match {
        case char if char == initialQuote => StringToken(currentString.substring(1), from, to + 1)
        case _ => processString(currentString + char, from, to + 1, iterator)
      }
    }
  }

  @tailrec
  private def processIdentifier(currentValue: String, from: Int, to: Int, iterator: Iterator[Char]): Token = {
    readCharacter(iterator) match {
      case None => IdentifierToken(currentValue, from, to)
      case Some(char) => char match {
        case char if char.toString matches "[_0-9a-zA-Z]" => processIdentifier(currentValue + char.toString, from, to + 1, iterator)
        case char if Lexer.keywords.contains(currentValue) && char.isWhitespace => KeywordToken(currentValue, from, to)
        case _ => IdentifierToken(currentValue, from, to)
      }
    }
  }
}
