package lexer

import token.{AssignmentOperator, EndOfFile, IdentifierToken, KeywordToken, LexicalRange, StatementDelimiter, StringToken}
import org.scalatest.funspec.AnyFunSpec

class LexerTests extends AnyFunSpec {
  describe("getTokens") {
    it("should return the tokens of a nicely formatted file") {
      val fileContentAsString = "let x = \"Hello world!\";"
      assert((new Lexer).getTokens(fileContentAsString.iterator) == List(
        KeywordToken("let", 1, 3, LexicalRange(1, 3, 1, 1)),
        IdentifierToken("x", 5, 5, LexicalRange(5, 5, 1, 1)),
        AssignmentOperator(7, 7, LexicalRange(7, 7, 1, 1)),
        StringToken("Hello world!", 9, 22, LexicalRange(9, 22, 1, 1)),
        StatementDelimiter(23, 23, LexicalRange(23, 23, 1, 1)),
        EndOfFile(24, 24, LexicalRange(24, 24, 1, 1))
      ))
    }

    it("should return the tokens of a badly formatted file") {
      val fileContentAsString =
        """
        let x="Hello world!" ;
      """.stripMargin
      assert((new Lexer).getTokens(fileContentAsString.iterator.buffered) == List(
        KeywordToken("let", 1, 3, LexicalRange(1, 3, 1, 1)),
        IdentifierToken("x", 5, 5, LexicalRange(5, 5, 1, 1)),
        AssignmentOperator(6, 6, LexicalRange(6, 6, 1, 1)),
        StringToken("Hello world!", 7, 20, LexicalRange(7, 20, 1, 1)),
        StatementDelimiter(22, 22, LexicalRange(22, 22, 1, 1)),
        EndOfFile
      ))
    }
  }
}
