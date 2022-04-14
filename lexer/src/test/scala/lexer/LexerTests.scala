package lexer

import org.austral.ingsis.printscript.common.{LexicalRange, Token}
import org.scalatest.funspec.AnyFunSpec
import token.types.{Assignment, ClosedParenthesis, Colon, Const, EndOfFile, Identifier, Let, OpenParenthesis, Println, Semicolon, StringDataType, StringValue}

class LexerTests extends AnyFunSpec {
  describe("lex method") {
    describe("variable declaration") {
      it("should return the tokens 1") {
        val fileContentAsString = "let x:String;"
        assert(new Lexer(fileContentAsString).lex == List(
          new Token(Let, 0, 3, new LexicalRange(1, 1, 3, 1)),
          new Token(Identifier, 4, 5, new LexicalRange(5, 1, 5, 1)),
          new Token(Colon, 5, 6, new LexicalRange(6, 1, 6, 1)),
          new Token(StringDataType, 6, 12, new LexicalRange(7, 1, 12, 1)),
          new Token(Semicolon, 12, 13, new LexicalRange(13, 1, 13, 1)),
          new Token(EndOfFile, 13, 13, new LexicalRange(14, 1, 14, 1))
        ))
      }

      it("should return the tokens 2") {
        val fileContentAsString = "const x:String;"
        assert(new Lexer(fileContentAsString).lex == List(
          new Token(Const, 0, 5, new LexicalRange(1, 1, 5, 1)),
          new Token(Identifier, 6, 7, new LexicalRange(7, 1, 7, 1)),
          new Token(Colon, 7, 8, new LexicalRange(8, 1, 8, 1)),
          new Token(StringDataType, 8, 14, new LexicalRange(9, 1, 14, 1)),
          new Token(Semicolon, 14, 15, new LexicalRange(15, 1, 15, 1)),
          new Token(EndOfFile, 15, 15, new LexicalRange(16, 1, 16, 1))
        ))
      }
    }

    describe("with assignation statement") {
      it("should return the tokens 1") {
        val fileContentAsString = "x=\"Hello world!\";"
        assert(new Lexer(fileContentAsString).lex == List(
          new Token(Identifier, 0, 1, new LexicalRange(1, 1, 1, 1)),
          new Token(Assignment, 1, 2, new LexicalRange(2, 1, 2, 1)),
          new Token(StringValue, 2, 16, new LexicalRange(3, 1, 16, 1)),
          new Token(Semicolon, 16, 17, new LexicalRange(17, 1, 17, 1)),
          new Token(EndOfFile, 17, 17, new LexicalRange(18, 1, 18, 1))
        ))
      }
    }

    describe("with declaration and assignation statement") {
      it("should return the tokens 1") {
        val fileContentAsString = "let x:String=\"Hello world!\";"
        assert(new Lexer(fileContentAsString).lex == List(
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

      it("should return the tokens 2") {
        val fileContentAsString = "const x:String = \"Hello world!\";"
        assert(new Lexer(fileContentAsString).lex == List(
          new Token(Const, 0, 5, new LexicalRange(1, 1, 5, 1)),
          new Token(Identifier, 6, 7, new LexicalRange(7, 1, 7, 1)),
          new Token(Colon, 7, 8, new LexicalRange(8, 1, 8, 1)),
          new Token(StringDataType, 8, 14, new LexicalRange(9, 1, 14, 1)),
          new Token(Assignment, 15, 16, new LexicalRange(16, 1, 16, 1)),
          new Token(StringValue, 17, 31, new LexicalRange(18, 1, 31, 1)),
          new Token(Semicolon, 31, 32, new LexicalRange(32, 1, 32, 1)),
          new Token(EndOfFile, 32, 32, new LexicalRange(33, 1, 33, 1))
        ))
      }
    }

    describe("with println statement") {
      it("should return the tokens 1") {
        val fileContentAsString = "println(\"Hello world!\");"
        assert(new Lexer(fileContentAsString).lex == List(
          new Token(Println, 0, 7, new LexicalRange(1, 1, 7, 1)),
          new Token(OpenParenthesis, 7, 8, new LexicalRange(8, 1, 8, 1)),
          new Token(StringValue, 8, 22, new LexicalRange(9, 1, 22, 1)),
          new Token(ClosedParenthesis, 22, 23, new LexicalRange(23, 1, 23, 1)),
          new Token(Semicolon, 23, 24, new LexicalRange(24, 1, 24, 1)),
          new Token(EndOfFile, 24, 24, new LexicalRange(25, 1, 25, 1))
        ))
      }
    }

    describe("errors") {
      it("should throw an exception if using a reserved word as a variable name") {
        val fileContentAsString = "let println:Number = 10;"
        val thrown = intercept[Exception] {
          new Lexer(fileContentAsString).lex
        }
        assert(thrown.getMessage === "Attempting to use a reserved word as an identifier name in line 1, column 5")
      }

      it("should throw an exception given an invalid value") {
        val fileContentAsString = "let x:String = !;"
        val thrown = intercept[Exception] {
          new Lexer(fileContentAsString).lex
        }
        assert(thrown.getMessage === "Unknown character at line 1, column 16")
      }

      it("should throw an exception given nonsensical content") {
        val fileContentAsString = "#!$%"
        val thrown = intercept[Exception] {
          new Lexer(fileContentAsString).lex
        }
        assert(thrown.getMessage === "Unknown character at line 1, column 1")
      }

      it("should throw an exception using a reserved word as an identifier") {
        val fileContentAsString = "let println:Number = 10;"
        val thrown = intercept[Exception] {
          new Lexer(fileContentAsString).lex
        }
        assert(thrown.getMessage === "Attempting to use a reserved word as an identifier name in line 1, column 5")
      }
    }
  }
}
