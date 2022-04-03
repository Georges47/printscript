package lexer

import org.austral.ingsis.printscript.common.{LexicalRange, Token}
import org.scalatest.funspec.AnyFunSpec
import token.types.{Assignment, Colon, EndOfFile, Identifier, Let, Semicolon, StringDataType, StringValue}

class LexerTests extends AnyFunSpec {
  describe("getTokens method") {
    describe("with declaration and assignation statement") {
      it("should return the tokens 1") {
        val fileContentAsString = "let x:String=\"Hello world!\";"
        assert((new Lexer).getTokens(fileContentAsString.iterator) == List(
          new Token(Let, 0, 3, new LexicalRange(1, 1, 3, 1)),
          new Token(Identifier, 4, 5, new LexicalRange(5, 1, 5, 1)),
          new Token(Colon, 5, 6, new LexicalRange(6, 1, 6, 1)),
          new Token(StringDataType, 6, 12, new LexicalRange(7, 1, 12, 1)),
          new Token(Assignment, 12, 13, new LexicalRange(13, 1, 13, 1)),
          new Token(StringValue, 13, 27, new LexicalRange(14, 1, 27, 1)),
          new Token(Semicolon, 27, 28, new LexicalRange(28, 1, 28, 1)),
          new Token(EndOfFile, 28, 28, new LexicalRange(29, 1, 29, 1))
        ))
      }
    }
  }
}
