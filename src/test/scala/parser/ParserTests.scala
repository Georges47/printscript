package parser

import org.austral.ingsis.printscript.common.{LexicalRange, Token}
import org.scalatest.funspec.AnyFunSpec
import token.types.{Assignment, ClosedParenthesis, Colon, EndOfFile, Identifier, Let, OpenParenthesis, Println, Semicolon, StringDataType, StringValue}

class ParserTests extends AnyFunSpec {
  describe("parse method") {
    val parser = new Parser
//    val ast = parser.parse(fileContent, tokens)
    describe("variable declaration") {

    }
  }
}

