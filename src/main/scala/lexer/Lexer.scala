package lexer

import org.austral.ingsis.printscript.common.{LexicalRange, Token}
import token.types._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Lexer {
  val keywords = List("let", "println")
  def isDigit(c: Char): Boolean = c.isDigit
  def isIdentifier(c: Char): Boolean = c.toString matches "[_0-9a-zA-Z]"
}

class Lexer {
  def getTokens(iterator: Iterator[Char]): List[Token] = {
    val tokens: ListBuffer[Token] = ListBuffer.empty
    var currentIndex = 0
    var currentLexicalRange = new LexicalRange(1, 1, 1, 1)
    while (iterator.hasNext) {
      val newToken = getToken(currentIndex, currentLexicalRange, iterator)
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
      currentLexicalRange: LexicalRange,
      iterator: Iterator[Char]
  ): Token = {
    popCharacter(iterator) match {
      case None =>
        new Token(
          EndOfFile,
          currentIndex - 1,
          currentIndex - 1,
          currentLexicalRange
        )
      case Some(char) =>
        char match {
          case char if Lexer.isDigit(char) =>
            processNumber(
              char.toString,
              currentIndex,
              currentIndex,
              currentLexicalRange,
              iterator
            )
          case char if Lexer.isIdentifier(char) =>
            processIdentifier(
              char.toString,
              currentIndex,
              currentIndex,
              currentLexicalRange,
              iterator
            )
          case '(' =>
            new Token(
              OpenParenthesis,
              currentIndex,
              currentIndex + 1,
              currentLexicalRange
            )
          case ')' =>
            new Token(
              ClosedParenthesis,
              currentIndex,
              currentIndex + 1,
              currentLexicalRange
            )
          case '-' =>
            new Token(
              Minus,
              currentIndex,
              currentIndex + 1,
              currentLexicalRange
            )
          case '+' =>
            new Token(Plus, currentIndex, currentIndex + 1, currentLexicalRange)
          case '*' =>
            new Token(
              Asterisk,
              currentIndex,
              currentIndex + 1,
              currentLexicalRange
            )
          case '/' =>
            new Token(
              FrontSlash,
              currentIndex,
              currentIndex + 1,
              currentLexicalRange
            )
          case '\'' | '"' =>
            processString(
              char.toString,
              currentIndex,
              currentIndex + 1,
              currentLexicalRange,
              iterator
            )
          case '=' =>
            new Token(
              Assignment,
              currentIndex,
              currentIndex + 1,
              currentLexicalRange
            )
          case ':' =>
            new Token(
              Colon,
              currentIndex,
              currentIndex + 1,
              currentLexicalRange
            )
          case ';' =>
            new Token(
              Semicolon,
              currentIndex,
              currentIndex + 1,
              currentLexicalRange
            )
          case '\n' =>
            new Token(
              Newline,
              currentIndex,
              currentIndex + 1,
              currentLexicalRange
            )
          case ' ' =>
            getToken(
              currentIndex + 1,
              new LexicalRange(
                currentLexicalRange.getStartCol + 1,
                currentLexicalRange.getStartLine,
                currentLexicalRange.getEndCol + 1,
                currentLexicalRange.getEndLine
              ),
              iterator
            )
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
  private def processNumber(
      currentNumber: String,
      from: Int,
      to: Int,
      lexicalRange: LexicalRange,
      iterator: Iterator[Char]
  ): Token = {
    readCharacter(iterator) match {
      case None => throw new Exception(s"Error at $to")
      case Some(char) =>
        char match {
          case char if Lexer.isDigit(char) | char == '.' =>
            iterator.next
            processNumber(
              currentNumber + char.toString,
              from,
              to + 1,
              new LexicalRange(
                lexicalRange.getStartCol,
                lexicalRange.getStartLine,
                lexicalRange.getEndCol + 1,
                lexicalRange.getEndLine
              ),
              iterator
            )
          case _ =>
            new Token(token.types.NumberValue, from, to + 1, lexicalRange)
        }
    }
  }

  @tailrec
  private def processString(
      currentString: String,
      from: Int,
      to: Int,
      lexicalRange: LexicalRange,
      iterator: Iterator[Char]
  ): Token = {
    val initialQuote = currentString.head
    popCharacter(iterator) match {
      case None => throw new Exception("Malformed string, no closing quote")
      case Some(char) =>
        char match {
          case char if char == initialQuote =>
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
            processString(
              currentString + char,
              from,
              to + 1,
              new LexicalRange(
                lexicalRange.getStartCol,
                lexicalRange.getStartLine,
                lexicalRange.getEndCol + 1,
                lexicalRange.getEndLine
              ),
              iterator
            )
        }
    }
  }

  @tailrec
  private def processIdentifier(
      currentValue: String,
      from: Int,
      to: Int,
      lexicalRange: LexicalRange,
      iterator: Iterator[Char]
  ): Token = {
    readCharacter(iterator) match {
      case None =>
        currentValue match { // no se que tan necesaria es esta parte
          case "String" => new Token(StringDataType, from, to + 1, lexicalRange)
          case "Number" => new Token(NumberDataType, from, to + 1, lexicalRange)
          case _        => new Token(Identifier, from, to + 1, lexicalRange)
        }
      case Some(char) =>
        char match {
          case char if char.toString matches "[_0-9a-zA-Z]" =>
            iterator.next
            processIdentifier(
              currentValue + char.toString,
              from,
              to + 1,
              new LexicalRange(
                lexicalRange.getStartCol,
                lexicalRange.getStartLine,
                lexicalRange.getEndCol + 1,
                lexicalRange.getEndLine
              ),
              iterator
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
          case _ => new Token(Identifier, from, to + 1, lexicalRange)
        }
    }
  }
}
