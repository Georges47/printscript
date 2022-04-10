package parser

import lexer.Lexer
import org.austral.ingsis.printscript.common.{LexicalRange, Token}
import org.scalatest.funspec.AnyFunSpec
import token.types.{Assignment, ClosedParenthesis, Colon, EndOfFile, Identifier, Let, NumberDataType, OpenParenthesis, Println, Semicolon, StringDataType, StringValue}

class ParserTests extends AnyFunSpec {
  describe("parse method") {
    val parser = new Parser(List(StringDataType, NumberDataType))
//    val ast = parser.parse(fileContent, tokens)
    describe("variable declaration") {

    }

    describe("error handling") {
      it("should throw an exception given an unknown data type") {
        val fileContentAsString = "let x:Boolean = true;"
        val tokens = new Lexer(fileContentAsString).lex
        val thrown = intercept[Exception] {
          parser.parse(fileContentAsString, tokens)
        }
        assert(thrown.getMessage === "Unknown data type in line 1, column 7")
      }
    }
  }
}

