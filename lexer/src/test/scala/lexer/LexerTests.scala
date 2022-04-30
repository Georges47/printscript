package lexer

import org.austral.ingsis.printscript.common.{LexicalRange, Token}
import org.scalatest.funspec.AnyFunSpec
import token.types._

class LexerTests extends AnyFunSpec {
  describe("lex method") {
    describe("identifier declaration") {
      it("should return a list of tokens of a string variable declaration statement") {
        val fileContentAsString = "let x:string;"
        assert(new Lexer(fileContentAsString).lex == List(
          new Token(Let, 0, 3, new LexicalRange(1, 1, 3, 1)),
          new Token(Whitespace, 3, 4, new LexicalRange(4, 1, 4, 1)),
          new Token(Identifier, 4, 5, new LexicalRange(5, 1, 5, 1)),
          new Token(Colon, 5, 6, new LexicalRange(6, 1, 6, 1)),
          new Token(StringDataType, 6, 12, new LexicalRange(7, 1, 12, 1)),
          new Token(Semicolon, 12, 13, new LexicalRange(13, 1, 13, 1)),
          new Token(EndOfFile, 13, 13, new LexicalRange(14, 1, 14, 1))
        ))
      }

      it("should return a list of tokens of a string constant declaration statement") {
        val fileContentAsString = "let x:boolean;"
        assert(new Lexer(fileContentAsString).lex == List(
          new Token(Let, 0, 3, new LexicalRange(1, 1, 3, 1)),
          new Token(Whitespace, 3, 4, new LexicalRange(4, 1, 4, 1)),
          new Token(Identifier, 4, 5, new LexicalRange(5, 1, 5, 1)),
          new Token(Colon, 5, 6, new LexicalRange(6, 1, 6, 1)),
          new Token(BooleanDataType, 6, 13, new LexicalRange(7, 1, 13, 1)),
          new Token(Semicolon, 13, 14, new LexicalRange(14, 1, 14, 1)),
          new Token(EndOfFile, 14, 14, new LexicalRange(15, 1, 15, 1))
        ))
      }
    }

    describe("assignation statement") {
      it("should return a list of tokens of a string assignment statement") {
        val fileContentAsString = "x=\"Hello world!\";"
        assert(new Lexer(fileContentAsString).lex == List(
          new Token(Identifier, 0, 1, new LexicalRange(1, 1, 1, 1)),
          new Token(Assignment, 1, 2, new LexicalRange(2, 1, 2, 1)),
          new Token(StringValue, 2, 16, new LexicalRange(3, 1, 16, 1)),
          new Token(Semicolon, 16, 17, new LexicalRange(17, 1, 17, 1)),
          new Token(EndOfFile, 17, 17, new LexicalRange(18, 1, 18, 1))
        ))
      }

      it("should return a list of tokens of a number assignment statement") {
        val fileContentAsString = "x=123;"
        assert(new Lexer(fileContentAsString).lex == List(
          new Token(Identifier, 0, 1, new LexicalRange(1, 1, 1, 1)),
          new Token(Assignment, 1, 2, new LexicalRange(2, 1, 2, 1)),
          new Token(NumberValue, 2, 5, new LexicalRange(3, 1, 5, 1)),
          new Token(Semicolon, 5, 6, new LexicalRange(6, 1, 6, 1)),
          new Token(EndOfFile, 6, 6, new LexicalRange(7, 1, 7, 1))
        ))
      }
    }

    describe("declaration and assignation statement") {
      it("should return a list of tokens of a string variable declaration and assignation") {
        val fileContentAsString = "let x:string=\"Hello world!\";"
        assert(new Lexer(fileContentAsString).lex == List(
          new Token(Let, 0, 3, new LexicalRange(1, 1, 3, 1)),
          new Token(Whitespace, 3, 4, new LexicalRange(4, 1, 4, 1)),
          new Token(Identifier, 4, 5, new LexicalRange(5, 1, 5, 1)),
          new Token(Colon, 5, 6, new LexicalRange(6, 1, 6, 1)),
          new Token(StringDataType, 6, 12, new LexicalRange(7, 1, 12, 1)),
          new Token(Assignment, 12, 13, new LexicalRange(13, 1, 13, 1)),
          new Token(StringValue, 13, 27, new LexicalRange(14, 1, 27, 1)),
          new Token(Semicolon, 27, 28, new LexicalRange(28, 1, 28, 1)),
          new Token(EndOfFile, 28, 28, new LexicalRange(29, 1, 29, 1))
        ))
      }

      it("should return a list of tokens of a string constant declaration and assignation") {
        val fileContentAsString = "const x:number = 1;"
        assert(new Lexer(fileContentAsString).lex == List(
          new Token(Const, 0, 5, new LexicalRange(1, 1, 5, 1)),
          new Token(Whitespace, 5, 6, new LexicalRange(6, 1, 6, 1)),
          new Token(Identifier, 6, 7, new LexicalRange(7, 1, 7, 1)),
          new Token(Colon, 7, 8, new LexicalRange(8, 1, 8, 1)),
          new Token(NumberDataType, 8, 14, new LexicalRange(9, 1, 14, 1)),
          new Token(Whitespace, 14, 15, new LexicalRange(15, 1, 15, 1)),
          new Token(Assignment, 15, 16, new LexicalRange(16, 1, 16, 1)),
          new Token(Whitespace, 16, 17, new LexicalRange(17, 1, 17, 1)),
          new Token(NumberValue, 17, 18, new LexicalRange(18, 1, 18, 1)),
          new Token(Semicolon, 18, 19, new LexicalRange(19, 1, 19, 1)),
          new Token(EndOfFile, 19, 19, new LexicalRange(20, 1, 20, 1))
        ))
      }
    }

    describe("println statement") {
      it("should return a list of tokens of a println statement") {
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

    describe("readInput statement") {
      it("should return a list of tokens of a readInput statement") {
        val fileContentAsString = "readInput(\"Hello world!\");"
        assert(new Lexer(fileContentAsString).lex == List(
          new Token(ReadInput, 0, 9, new LexicalRange(1, 1, 9, 1)),
          new Token(OpenParenthesis, 9, 10, new LexicalRange(10, 1, 10, 1)),
          new Token(StringValue, 10, 24, new LexicalRange(11, 1, 24, 1)),
          new Token(ClosedParenthesis, 24, 25, new LexicalRange(25, 1, 25, 1)),
          new Token(Semicolon, 25, 26, new LexicalRange(26, 1, 26, 1)),
          new Token(EndOfFile, 26, 26, new LexicalRange(27, 1, 27, 1))
        ))
      }
    }

    describe("if/else statement") {
      it("should return a list of tokens of an if statement") {
        val fileContentAsString = "if(true){\t};"
        assert(new Lexer(fileContentAsString).lex == List(
          new Token(If, 0, 2, new LexicalRange(1, 1, 2, 1)),
          new Token(OpenParenthesis, 2, 3, new LexicalRange(3, 1, 3, 1)),
          new Token(BooleanValue, 3, 7, new LexicalRange(4, 1, 7, 1)),
          new Token(ClosedParenthesis, 7, 8, new LexicalRange(8, 1, 8, 1)),
          new Token(OpenBracket, 8, 9, new LexicalRange(9, 1, 9, 1)),
          new Token(Tab, 9, 10, new LexicalRange(10, 1, 10, 1)),
          new Token(ClosedBracket, 10, 11, new LexicalRange(12, 1, 12, 1)),
          new Token(Semicolon, 11, 12, new LexicalRange(13, 1, 13, 1)),
          new Token(EndOfFile, 12, 12, new LexicalRange(14, 1, 14, 1))
        ))
      }

      it("should return a list of tokens of an if/else statement") {
        val fileContentAsString = "if(true){}else{};\n"
        assert(new Lexer(fileContentAsString).lex == List(
          new Token(If, 0, 2, new LexicalRange(1, 1, 2, 1)),
          new Token(OpenParenthesis, 2, 3, new LexicalRange(3, 1, 3, 1)),
          new Token(BooleanValue, 3, 7, new LexicalRange(4, 1, 7, 1)),
          new Token(ClosedParenthesis, 7, 8, new LexicalRange(8, 1, 8, 1)),
          new Token(OpenBracket, 8, 9, new LexicalRange(9, 1, 9, 1)),
          new Token(ClosedBracket, 9, 10, new LexicalRange(10, 1, 10, 1)),
          new Token(Else, 10, 14, new LexicalRange(11, 1, 14, 1)),
          new Token(OpenBracket, 14, 15, new LexicalRange(15, 1, 15, 1)),
          new Token(ClosedBracket, 15, 16, new LexicalRange(16, 1, 16, 1)),
          new Token(Semicolon, 16, 17, new LexicalRange(17, 1, 17, 1)),
          new Token(Newline, 17, 18, new LexicalRange(18, 1, 18, 1)),
          new Token(EndOfFile, 18, 18, new LexicalRange(1, 2, 1, 2))
        ))
      }
    }

    describe("operations") {
      it("should return a list of tokens of a math operation statement") {
        val fileContentAsString = "(1-2+3*4)/5;"
        assert(new Lexer(fileContentAsString).lex == List(
          new Token(OpenParenthesis, 0, 1, new LexicalRange(1, 1, 1, 1)),
          new Token(NumberValue, 1, 2, new LexicalRange(2, 1, 2, 1)),
          new Token(Minus, 2, 3, new LexicalRange(3, 1, 3, 1)),
          new Token(NumberValue, 3, 4, new LexicalRange(4, 1, 4, 1)),
          new Token(Plus, 4, 5, new LexicalRange(5, 1, 5, 1)),
          new Token(NumberValue, 5, 6, new LexicalRange(6, 1, 6, 1)),
          new Token(Asterisk, 6, 7, new LexicalRange(7, 1, 7, 1)),
          new Token(NumberValue, 7, 8, new LexicalRange(8, 1, 8, 1)),
          new Token(ClosedParenthesis, 8, 9, new LexicalRange(9, 1, 9, 1)),
          new Token(FrontSlash, 9, 10, new LexicalRange(10, 1, 10, 1)),
          new Token(NumberValue, 10, 11, new LexicalRange(11, 1, 11, 1)),
          new Token(Semicolon, 11, 12, new LexicalRange(12, 1, 12, 1)),
          new Token(EndOfFile, 12, 12, new LexicalRange(13, 1, 13, 1)),
        ))
      }

      it("should return a list of tokens of a boolean operation statement") {
        val fileContentAsString = "true &true|false;"
        assert(new Lexer(fileContentAsString).lex == List(
          new Token(BooleanValue, 0, 4, new LexicalRange(1, 1, 4, 1)),
          new Token(Whitespace, 4, 5, new LexicalRange(5, 1, 5, 1)),
          new Token(And, 5, 6, new LexicalRange(6, 1, 6, 1)),
          new Token(BooleanValue, 6, 10, new LexicalRange(7, 1, 10, 1)),
          new Token(Or, 10, 11, new LexicalRange(11, 1, 11, 1)),
          new Token(BooleanValue, 11, 16, new LexicalRange(12, 1, 16, 1)),
          new Token(Semicolon, 16, 17, new LexicalRange(17, 1, 17, 1)),
          new Token(EndOfFile, 17, 17, new LexicalRange(18, 1, 18, 1)),
        ))
      }
    }

    describe("errors") {
      it("should throw an exception if using a reserved word as a variable name") {
        val fileContentAsString = "let println:number = 10;"
        val thrown = intercept[Exception] {
          new Lexer(fileContentAsString).lex
        }
        assert(thrown.getMessage === "Attempting to use a reserved word as an identifier name in line 1, column 5")
      }

      it("should throw an exception given an invalid value") {
        val fileContentAsString = "let x:string = !;"
        val thrown = intercept[Exception] {
          new Lexer(fileContentAsString).lex
        }
        assert(thrown.getMessage === "Unknown character ! at line 1, column 16")
      }

      it("should throw an exception given nonsensical content") {
        val fileContentAsString = "#!$%"
        val thrown = intercept[Exception] {
          new Lexer(fileContentAsString).lex
        }
        assert(thrown.getMessage === "Unknown character # at line 1, column 1")
      }

      it("should throw an exception using a reserved word as an identifier") {
        val fileContentAsString = "let println:number = 10;"
        val thrown = intercept[Exception] {
          new Lexer(fileContentAsString).lex
        }
        assert(thrown.getMessage === "Attempting to use a reserved word as an identifier name in line 1, column 5")
      }
    }
  }
}
