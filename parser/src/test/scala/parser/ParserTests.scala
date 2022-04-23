package parser

import abstractSyntaxTree.{AbstractSyntaxTree, Node}
import lexer.Lexer
import org.austral.ingsis.printscript.common.{LexicalRange, Token}
import org.scalatest.funspec.AnyFunSpec
import token.types._

class ParserTests extends AnyFunSpec {
  val parser = new Parser()

  describe("parse method") {

    describe("constant declaration and assignment") {
      val fileContentAsString = "const x:number = 10;"

      val tokens = List(
        new Token(Const, 0, 5, new LexicalRange(1, 1, 5, 1)),
        new Token(Identifier, 6, 7, new LexicalRange(7, 1, 7, 1)),
        new Token(Colon, 7, 8, new LexicalRange(8, 1, 8, 1)),
        new Token(NumberDataType, 8, 14, new LexicalRange(9, 1, 14, 1)),
        new Token(Assignment, 15, 16, new LexicalRange(16, 1, 16, 1)),
        new Token(NumberValue, 17, 19, new LexicalRange(18, 1, 19, 1)),
        new Token(Semicolon, 19, 20, new LexicalRange(20, 1, 20, 1)),
        new Token(EndOfFile, 20, 20, new LexicalRange(21, 1, 21, 1))
      )

      println(parser.parse(fileContentAsString, tokens).nodes.head.nodes(2).nodes.head.root)

      val expectedAST = AbstractSyntaxTree(Node("Program", Program), List(
        AbstractSyntaxTree(Node("DeclarationAndAssignment", DeclarationAndAssignment), List(
          AbstractSyntaxTree(Node("x", ConstantIdentifier)),
          AbstractSyntaxTree(Node("number", NumberDataType, Some(new LexicalRange(9, 1, 14, 1)))),
          AbstractSyntaxTree(Node("Expression", Expression), List(
            AbstractSyntaxTree(Node("10", NumberValue, Some(new LexicalRange(18, 1, 19, 1))))
          )),
        )),
        AbstractSyntaxTree(Node("EndOfFile", EndOfFile))
      ))

      println(expectedAST.nodes.head.nodes(2).nodes.head.root)

      assert(parser.parse(fileContentAsString, tokens) == expectedAST)
    }

    describe("error handling") {
      it("should throw an exception given an unknown data type") {
        val fileContentAsString = "let x:char = 'a';"
        val tokens = new Lexer(fileContentAsString).lex
        val thrown = intercept[Exception] {
          parser.parse(fileContentAsString, tokens)
        }
        assert(thrown.getMessage === "Unknown data type in line 1, column 7")
      }
    }

  }
}