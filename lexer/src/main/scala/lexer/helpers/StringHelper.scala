package lexer.helpers

import lexer.helpers
import org.austral.ingsis.printscript.common.{LexicalRange, Token}

import scala.annotation.tailrec

case class StringHelper() extends LexerHelper {
  override def lex(
      currentString: String,
      from: Int,
      to: Int,
      lexicalRange: LexicalRange,
      fileContent: String
  ): LexerHelperResponse = {
    helper(currentString, from, to + 1, lexicalRange, fileContent)
  }

  @tailrec
  private def helper(
      currentString: String,
      from: Int,
      to: Int,
      lexicalRange: LexicalRange,
      fileContent: String
  ): LexerHelperResponse = {
    var content = fileContent
    val initialQuote = currentString.head
    val newRange = LexerHelper.rangeAddEndColumn(lexicalRange, 1)
    content.head match {
      case char if char == initialQuote =>
        content = content.substring(1)
        helpers.LexerHelperResponse(
          content,
          new Token(token.types.StringValue, from, to + 1, newRange)
        )
      case _ =>
        val newChar = content.head
        content = content.substring(1)
        helper(currentString + newChar, from, to + 1, newRange, content)
    }
  }
}
