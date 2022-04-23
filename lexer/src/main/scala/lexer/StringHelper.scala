package lexer

import org.austral.ingsis.printscript.common.{LexicalRange, Token}

import scala.annotation.tailrec

case class StringHelper() extends LexerHelper {
  override def lex(currentString: String, from: Int, to: Int, lexicalRange: LexicalRange, fileContent: String): HelperResponse = {
    helper(currentString, from, to + 1, lexicalRange, fileContent)
  }

  @tailrec
  private def helper(currentString: String, from: Int, to: Int, lexicalRange: LexicalRange, fileContent: String): HelperResponse = {
    var content = fileContent
    val initialQuote = currentString.head
    content.head match {
      case char if char == initialQuote =>
        content = content.substring(1)
        HelperResponse(
          content,
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
        )
      case _ =>
        val newChar = content.head
        content = content.substring(1)
        helper(
          currentString + newChar,
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
    }
  }
}