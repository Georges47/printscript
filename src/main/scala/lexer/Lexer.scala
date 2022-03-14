package lexer

import token.{AssignmentOperator, DoubleToken, EndOfFile, IntegerToken, LiteralToken, OperatorToken, StatementDelimiter, StringToken, Token}

import scala.collection.mutable.ListBuffer

object Lexer {
  def isDigit(c: Char): Boolean = c.isDigit
  def isLiteral(c: Char): Boolean = c.toString matches "[_0-9a-zA-Z]"
  def isOperator(c: Char): Boolean = c.toString matches "[-+*/]"
}

class Lexer {
  def getTokens(bufferedIterator: BufferedIterator[Char]): List[Token] = {
    val tokens: ListBuffer[Token] = ListBuffer.empty
    while (bufferedIterator.hasNext) {
      tokens += getToken(bufferedIterator)
    }
    tokens.toList
  }

  def readCharacter(bufferedIterator: BufferedIterator[Char]): Option[Char] = {
    if (bufferedIterator.hasNext) Some(bufferedIterator.head) else None
  }

  def getToken(bufferedIterator: BufferedIterator[Char]): Token = {
    readCharacter(bufferedIterator) match {
      case None => EndOfFile
      case Some(c) => c match {
        case c if Lexer.isDigit(c) => parseNumber(c.toString, bufferedIterator)
        case c if Lexer.isLiteral(c) => parseLiteral(c.toString, bufferedIterator)
        case c if Lexer.isOperator(c) => bufferedIterator.next; OperatorToken(c)
        case '\'' | '"' => parseString(c.toString, bufferedIterator)
        case '=' => bufferedIterator.next; AssignmentOperator
        case ';' => bufferedIterator.next; StatementDelimiter
        case _ => bufferedIterator.next; getToken(bufferedIterator)
      }
    }
  }

  // TODO: manejar el caso de tener un double mal formado, por ejemplo 1.2.3
  // TODO: manejar el caso de tener un double sin parte decimal, por ejemplo 1. (?
  def parseNumber(currentNumber: String, bufferedIterator: BufferedIterator[Char]): Token = {
    bufferedIterator.next
    readCharacter(bufferedIterator) match {
      case None => if (currentNumber.contains('.')) DoubleToken(currentNumber.toDouble) else IntegerToken(currentNumber.toInt)
      case Some(c) => c match {
        case c if Lexer.isDigit(c) | c == '.' => parseNumber(currentNumber + c.toString, bufferedIterator)
        case _  => if (currentNumber.contains('.')) DoubleToken(currentNumber.toDouble) else IntegerToken(currentNumber.toInt)
      }
    }
  }

  // TODO: manejar escapear el tipo de quote que usa la string
  def parseString(currentString: String, bufferedIterator: BufferedIterator[Char]): Token = {
    val initialQuote = currentString.head
    bufferedIterator.next
    readCharacter(bufferedIterator) match {
      case None => throw new Exception("Malformed string, no closing quote")
      case Some(c) => c match {
        case c if c == initialQuote => bufferedIterator.next; StringToken(currentString + c)
        case _ => parseString(currentString + c, bufferedIterator)
      }
    }
  }

  // TODO: manejar el tipo de la variable
  def parseLiteral(currentValue: String, bufferedIterator: BufferedIterator[Char]): Token = {
    bufferedIterator.next
    readCharacter(bufferedIterator) match {
      case None => LiteralToken(currentValue)
      case Some(c) => c match {
        case c if c.toString matches "[_0-9a-zA-Z]" => parseLiteral(currentValue + c.toString, bufferedIterator)
        case _ => LiteralToken(currentValue)
      }
    }
  }
}
