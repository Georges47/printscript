package lexer

import token.{AssignmentOperator, EndOfFile, LiteralToken, StatementDelimiter, StringToken}
import org.scalatest.flatspec.AnyFlatSpec

class LexerTests extends AnyFlatSpec {
  it should "return the tokens of a nicely formatted file" in {
    val fileContentAsString =
      """
        let x = "Hello world!";
      """.stripMargin
    assert((new Lexer).getTokens(fileContentAsString.iterator.buffered) == List(
      LiteralToken("let"), LiteralToken("x"),
      AssignmentOperator, StringToken("\"Hello world!\""),
      StatementDelimiter, EndOfFile
    ))
  }

  it should "return the tokens of a badly formatted file" in {
    val fileContentAsString =
      """
        let x="Hello world!" ;
      """.stripMargin
    assert((new Lexer).getTokens(fileContentAsString.iterator.buffered) == List(
      LiteralToken("let"), LiteralToken("x"),
      AssignmentOperator, StringToken("\"Hello world!\""),
      StatementDelimiter, EndOfFile
    ))
  }
}
