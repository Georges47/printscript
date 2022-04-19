package parser

import abstractSyntaxTree.{AbstractSyntaxTree, Node}
import lexer.Lexer
import org.austral.ingsis.printscript.common.{LexicalRange, Token}
import org.scalatest.funspec.AnyFunSpec
import token.types.{Assignment, Colon, Const, ConstantIdentifier, DeclarationAndAssignment, EndOfFile, Expression, Identifier, Let, NumberDataType, NumberValue, Program, Semicolon, StringDataType}

class ParserTests extends AnyFunSpec {
  val dataTypes = List(StringDataType, NumberDataType)
  val parser = new Parser(dataTypes)

  describe("parse method") {
    describe("variable declaration and assignment") {
      it("should return the abstract syntax tree") {
//        val fileContentAsString = "let x:Number = 10;"
//        val fileContent = Using(Source.fromURL(getClass.getResource("/file"))) { source => source.mkString } // Source.fromURL(getClass.getResource("/file")).toString
        val fileContentAsString = "let x:Number = 10;" //fileContent.getOrElse(throw new Exception("Could not find test file"))

        val tokens = List(
          new Token(Let, 0, 3, new LexicalRange(1, 1, 3, 1)),
          new Token(Identifier, 4, 5, new LexicalRange(5, 1, 5, 1)),
          new Token(Colon, 5, 6, new LexicalRange(6, 1, 6, 1)),
          new Token(NumberDataType, 6, 12, new LexicalRange(7, 1, 12, 1)),
          new Token(Assignment, 13, 14, new LexicalRange(14, 1, 14, 1)),
          new Token(NumberValue, 15, 17, new LexicalRange(16, 1, 17, 1)),
          new Token(Semicolon, 17, 18, new LexicalRange(18, 1, 18, 1)),
          new Token(EndOfFile, 18, 18, new LexicalRange(19, 1, 19, 1))
        )


        assert(parser.parse(fileContentAsString, tokens) ==
          AbstractSyntaxTree(Node("Program", Program), List(
            AbstractSyntaxTree(Node("DeclarationAndAssignment", DeclarationAndAssignment), List(
              AbstractSyntaxTree(Node("x", Identifier)),
              AbstractSyntaxTree(Node("Number", NumberDataType)),
              AbstractSyntaxTree(Node("Expression", Expression), List(
                AbstractSyntaxTree(Node("10", NumberValue))
              )),
            ))
          ))
        )
      }
    }

    describe("constant declaration and assignment") {
      val fileContentAsString = "const x:Number = 10;"

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

      println(parser.parse(fileContentAsString, tokens).nodes(0).nodes(2).nodes(0).root)

      val expectedAST = AbstractSyntaxTree(Node("Program", Program), List(
        AbstractSyntaxTree(Node("DeclarationAndAssignment", DeclarationAndAssignment), List(
          AbstractSyntaxTree(Node("x", ConstantIdentifier)),
          AbstractSyntaxTree(Node("Number", NumberDataType, Some(new LexicalRange(9, 1, 14, 1)))),
          AbstractSyntaxTree(Node("Expression", Expression), List(
            AbstractSyntaxTree(Node("10", NumberValue, Some(new LexicalRange(18, 1, 19, 1))))
          )),
        )),
        AbstractSyntaxTree(Node("EndOfFile", EndOfFile))
      ))

      println(expectedAST.nodes(0).nodes(2).nodes(0).root)

      assert(parser.parse(fileContentAsString, tokens) == expectedAST)
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