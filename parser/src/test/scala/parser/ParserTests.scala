package parser

import abstractSyntaxTree.{AbstractSyntaxTree, Node}
import lexer.Lexer
import org.austral.ingsis.printscript.common.{LexicalRange, Token}
import org.scalatest.funspec.AnyFunSpec
import token.types._

class ParserTests extends AnyFunSpec {
  val parser = new Parser()

  describe("parse method") {
    describe("assignment") {
      it("should parse the tokens of a variable assignment") {
        val fileContentAsString = "x = 10;"

        val lexer = new Lexer(fileContentAsString)
        val tokens = lexer.lex

        val expectedAST = AbstractSyntaxTree(Node("Program", Program), List(
          AbstractSyntaxTree(Node("Assignment", Assignment), List(
            AbstractSyntaxTree(Node("x", Identifier, Some(new LexicalRange(1, 1, 1, 1)))),
            AbstractSyntaxTree(Node("Expression", Expression), List(
              AbstractSyntaxTree(Node("10", NumberValue, Some(new LexicalRange(5, 1, 6, 1))))
            )),
          )),
          AbstractSyntaxTree(Node("EndOfFile", EndOfFile))
        ))

        assert(parser.parse(fileContentAsString, tokens) == expectedAST)
      }
    }

    describe("declaration") {
      it("should parse the tokens of a variable declaration") {
        val fileContentAsString = "let x:number;"

        val lexer = new Lexer(fileContentAsString)
        val tokens = lexer.lex

        val expectedAST = AbstractSyntaxTree(Node("Program", Program), List(
          AbstractSyntaxTree(Node("Declaration", Declaration), List(
            AbstractSyntaxTree(Node("x", VariableIdentifier)),
            AbstractSyntaxTree(Node("number", NumberDataType, Some(new LexicalRange(7, 1, 12, 1))))
          )),
          AbstractSyntaxTree(Node("EndOfFile", EndOfFile))
        ))

        assert(parser.parse(fileContentAsString, tokens) == expectedAST)
      }
    }

    describe("declaration and assignment") {
      it("should parse the tokens of a constant declaration and assignment") {
        val fileContentAsString = "const x:number = 10;"

        val lexer = new Lexer(fileContentAsString)
        val tokens = lexer.lex

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

        assert(parser.parse(fileContentAsString, tokens) == expectedAST)
      }

      it("should parse the tokens of a variable declaration and assignment") {
        val fileContentAsString = "let x:number = 10;"

        val lexer = new Lexer(fileContentAsString)
        val tokens = lexer.lex

        val expectedAST = AbstractSyntaxTree(Node("Program", Program), List(
          AbstractSyntaxTree(Node("DeclarationAndAssignment", DeclarationAndAssignment), List(
            AbstractSyntaxTree(Node("x", VariableIdentifier)),
            AbstractSyntaxTree(Node("number", NumberDataType, Some(new LexicalRange(7, 1, 12, 1)))),
            AbstractSyntaxTree(Node("Expression", Expression), List(
              AbstractSyntaxTree(Node("10", NumberValue, Some(new LexicalRange(16, 1, 17, 1))))
            )),
          )),
          AbstractSyntaxTree(Node("EndOfFile", EndOfFile))
        ))

        assert(parser.parse(fileContentAsString, tokens) == expectedAST)
      }
    }

    describe("println statement") {
      it("should return an ast of a println statement") {
        val fileContentAsString = "println(\"Hello world!\");"

        val lexer = new Lexer(fileContentAsString)
        val tokens = lexer.lex

        val expectedAST = AbstractSyntaxTree(Node("Program", Program), List(
          AbstractSyntaxTree(Node("Println", Println), List(
            AbstractSyntaxTree(Node("Expression", Expression), List(
              AbstractSyntaxTree(Node("\"Hello world!\"", StringValue, Some(new LexicalRange(9, 1, 22, 1))))
            ))
          )),
          AbstractSyntaxTree(Node("EndOfFile", EndOfFile))
        ))

        assert(parser.parse(fileContentAsString, tokens) == expectedAST)
      }
    }

    describe("readInput statement") {
      it("should return an ast of a readInput statement") {
        val fileContentAsString = "readInput(\"Hello world!\");"

        val lexer = new Lexer(fileContentAsString)
        val tokens = lexer.lex

        val expectedAST = AbstractSyntaxTree(Node("Program", Program), List(
          AbstractSyntaxTree(Node("ReadInput", ReadInput), List(
            AbstractSyntaxTree(Node("\"Hello world!\"", StringValue, Some(new LexicalRange(11, 1, 24, 1))))
          )),
          AbstractSyntaxTree(Node("EndOfFile", EndOfFile))
        ))

        assert(parser.parse(fileContentAsString, tokens) == expectedAST)
      }

      it("should return an ast of a readInput variable declaration and assignation") {
        val fileContentAsString = "let x:string = readInput(\"Hello world!\");"

        val lexer = new Lexer(fileContentAsString)
        val tokens = lexer.lex

        val expectedAST = AbstractSyntaxTree(Node("Program", Program), List(
          AbstractSyntaxTree(Node("DeclarationAndAssignment", DeclarationAndAssignment), List(
            AbstractSyntaxTree(Node("x", VariableIdentifier)),
            AbstractSyntaxTree(Node("string", StringDataType, Some(new LexicalRange(7, 1, 12, 1)))),
            AbstractSyntaxTree(Node("ReadInput", ReadInput)),
            AbstractSyntaxTree(Node("\"Hello world!\"", StringValue, Some(new LexicalRange(26, 1, 39, 1))))
          )),
          AbstractSyntaxTree(Node("EndOfFile", EndOfFile))
        ))

        assert(parser.parse(fileContentAsString, tokens) == expectedAST)
      }

      it("should return an ast of a readInput constant declaration and assignation") {
        val fileContentAsString = "const x:string = readInput(\"Hello world!\");"

        val lexer = new Lexer(fileContentAsString)
        val tokens = lexer.lex

        val expectedAST = AbstractSyntaxTree(Node("Program", Program), List(
          AbstractSyntaxTree(Node("DeclarationAndAssignment", DeclarationAndAssignment), List(
            AbstractSyntaxTree(Node("x", ConstantIdentifier)),
            AbstractSyntaxTree(Node("string", StringDataType, Some(new LexicalRange(9, 1, 14, 1)))),
            AbstractSyntaxTree(Node("ReadInput", ReadInput)),
            AbstractSyntaxTree(Node("\"Hello world!\"", StringValue, Some(new LexicalRange(28, 1, 41, 1))))
          )),
          AbstractSyntaxTree(Node("EndOfFile", EndOfFile))
        ))

        assert(parser.parse(fileContentAsString, tokens) == expectedAST)
      }
    }

    describe("if/else statement") {
      it("should return an ast of an if statement") {
        val fileContentAsString = "if(true){}else{};\n"

        val lexer = new Lexer(fileContentAsString)
        val tokens = lexer.lex

        val expectedAST = AbstractSyntaxTree(Node("Program", Program), List(
          AbstractSyntaxTree(Node("If", If), List(
            AbstractSyntaxTree(Node("true", BooleanValue)),
            AbstractSyntaxTree(Node("Block", Block), List(
              AbstractSyntaxTree(Node("ClosedBracket", ClosedBracket))
            )),
            AbstractSyntaxTree(Node("Block", Block), List(
              AbstractSyntaxTree(Node("ClosedBracket", ClosedBracket))
            )),
          )),
          AbstractSyntaxTree(Node("EndOfFile", EndOfFile))
        ))

        assert(parser.parse(fileContentAsString, tokens) == expectedAST)
      }
    }

    describe("identifier statement") {
      it("should return an ast of an identifier operation") {
        val fileContentAsString = "x + 1;"

        val lexer = new Lexer(fileContentAsString)
        val tokens = lexer.lex

        val expectedAST = AbstractSyntaxTree(Node("Program", Program), List(
          AbstractSyntaxTree(Node("Expression", Expression), List(
            AbstractSyntaxTree(Node("x", Identifier, Some(new LexicalRange(1, 1, 1, 1)))),
            AbstractSyntaxTree(Node("+", Plus, Some(new LexicalRange(3, 1, 3, 1)))),
            AbstractSyntaxTree(Node("1", NumberValue, Some(new LexicalRange(5, 1, 5, 1)))),
          )),
          AbstractSyntaxTree(Node("EndOfFile", EndOfFile))
        ))

        assert(parser.parse(fileContentAsString, tokens) == expectedAST)
      }

      it("should return an ast of a single identifier") {
        val fileContentAsString = "x;"

        val lexer = new Lexer(fileContentAsString)
        val tokens = lexer.lex

        val expectedAST = AbstractSyntaxTree(Node("Program", Program), List(
          AbstractSyntaxTree(Node("Expression", Expression), List(
            AbstractSyntaxTree(Node("x", Identifier, Some(new LexicalRange(1, 1, 1, 1)))),
          )),
          AbstractSyntaxTree(Node("EndOfFile", EndOfFile))
        ))

        assert(parser.parse(fileContentAsString, tokens) == expectedAST)
      }
    }

    describe("operations") {
      it("should return an ast of a mathematical expression") {
        val fileContentAsString = "(1-2+3*4)/(5-1);"

        val lexer = new Lexer(fileContentAsString)
        val tokens = lexer.lex

        val expectedAST = AbstractSyntaxTree(Node("Program", Program), List(
          AbstractSyntaxTree(Node("Expression", Expression), List(
            AbstractSyntaxTree(Node("(", OpenParenthesis, Some(new LexicalRange(1, 1, 1, 1)))),
            AbstractSyntaxTree(Node("1", NumberValue, Some(new LexicalRange(2, 1, 2, 1)))),
            AbstractSyntaxTree(Node("-", Minus, Some(new LexicalRange(3, 1, 3, 1)))),
            AbstractSyntaxTree(Node("2", NumberValue, Some(new LexicalRange(4, 1, 4, 1)))),
            AbstractSyntaxTree(Node("+", Plus, Some(new LexicalRange(5, 1, 5, 1)))),
            AbstractSyntaxTree(Node("3", NumberValue, Some(new LexicalRange(6, 1, 6, 1)))),
            AbstractSyntaxTree(Node("*", Asterisk, Some(new LexicalRange(7, 1, 7, 1)))),
            AbstractSyntaxTree(Node("4", NumberValue, Some(new LexicalRange(8, 1, 8, 1)))),
            AbstractSyntaxTree(Node(")", ClosedParenthesis, Some(new LexicalRange(9, 1, 9, 1)))),
            AbstractSyntaxTree(Node("/", FrontSlash, Some(new LexicalRange(10, 1, 10, 1)))),
            AbstractSyntaxTree(Node("(", OpenParenthesis, Some(new LexicalRange(11, 1, 11, 1)))),
            AbstractSyntaxTree(Node("5", NumberValue, Some(new LexicalRange(12, 1, 12, 1)))),
            AbstractSyntaxTree(Node("-", Minus, Some(new LexicalRange(13, 1, 13, 1)))),
            AbstractSyntaxTree(Node("1", NumberValue, Some(new LexicalRange(14, 1, 14, 1)))),
            AbstractSyntaxTree(Node(")", ClosedParenthesis, Some(new LexicalRange(15, 1, 15, 1)))),
          )),
          AbstractSyntaxTree(Node("EndOfFile", EndOfFile))
        ))

        assert(parser.parse(fileContentAsString, tokens) == expectedAST)
      }
    }

    describe("errors") {
      it("should throw an exception given an unknown data type in a variable declaration and assignation") {
        val fileContentAsString = "let x:char = 'a';"
        val tokens = new Lexer(fileContentAsString).lex
        val thrown = intercept[Exception] {
          parser.parse(fileContentAsString, tokens)
        }
        assert(thrown.getMessage === "Unknown data type in line 1, column 7")
      }

      it("should throw an exception given an unknown data type in a constant declaration and assignation") {
        val fileContentAsString = "const x:char = 'a';"
        val tokens = new Lexer(fileContentAsString).lex
        val thrown = intercept[Exception] {
          parser.parse(fileContentAsString, tokens)
        }
        assert(thrown.getMessage === "Unknown data type in line 1, column 9")
      }

      it("should throw an exception given an unknown token") {
        val fileContentAsString = "!"
        val tokens = List(new Token(UnknownToken, 0, 1, new LexicalRange(1, 1, 1, 1)))
        val thrown = intercept[Exception] {
          parser.parse(fileContentAsString, tokens)
        }
        assert(thrown.getMessage === "Unknown token of type UnknownToken at line 1, column 1")
      }

      it("should throw an exception given no value in a constant declaration and assignation") {
        val fileContentAsString = "const x:string;"
        val tokens = new Lexer(fileContentAsString).lex
        val thrown = intercept[Exception] {
          parser.parse(fileContentAsString, tokens)
        }
        assert(thrown.getMessage === "Expected assignment operator at line 1, column 15")
      }
    }

  }
}