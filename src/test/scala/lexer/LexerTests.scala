package lexer

import token.{AssignmentOperator, EndOfFile, IdentifierToken, KeywordToken, StatementDelimiter, StringToken}
import org.scalatest.funspec.AnyFunSpec

class LexerTests extends AnyFunSpec {
  describe("getTokens") {
    it("should return the tokens of a nicely formatted file") {
      val fileContentAsString =
        """
        let x = "Hello world!";
      """.stripMargin
      assert((new Lexer).getTokens(fileContentAsString.iterator.buffered) == List(
        KeywordToken("let"), IdentifierToken("x"),
        AssignmentOperator, StringToken("Hello world!"),
        StatementDelimiter, EndOfFile
      ))
    }

    it("should return the tokens of a badly formatted file") {
      val fileContentAsString =
        """
        let x="Hello world!" ;
      """.stripMargin
      assert((new Lexer).getTokens(fileContentAsString.iterator.buffered) == List(
        KeywordToken("let"), IdentifierToken("x"),
        AssignmentOperator, StringToken("Hello world!"),
        StatementDelimiter, EndOfFile
      ))
    }
  }

}
