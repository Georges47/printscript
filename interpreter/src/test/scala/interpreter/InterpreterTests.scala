package interpreter

import lexer.Lexer
import org.austral.ingsis.printscript.common.{LexicalRange, Token}
import org.scalatest.funspec.AnyFunSpec
import parser.Parser
import token.types.{BooleanValue, ClosedBracket, ClosedParenthesis, EndOfFile, If, OpenBracket, OpenParenthesis, Semicolon, Tab}

import scala.io.Source
import scala.util.Using

class InterpreterTests extends AnyFunSpec {
  val interpreter = new Interpreter

  it("should interpret a file") {
    val fileContent = Using(Source.fromURL(getClass.getResource("/file"))) { source => source.mkString } // Source.fromURL(getClass.getResource("/file")).toString
    val fileContentAsString = fileContent.getOrElse(throw new Exception("Could not find test file"))

    val lexer = new Lexer(fileContentAsString)
    val tokens = lexer.lex

    val parser = new Parser()
    val abstractSyntaxTree = parser.parse(fileContentAsString, tokens)

    interpreter.interpret(abstractSyntaxTree)

    assert(interpreter.expectVariableToExistWithValueAndDataType("x", "number", "0.50"))
    assert(interpreter.expectVariableToExistWithValueAndDataType("y", "number", "4.50"))
    assert(interpreter.expectVariableToExistWithValueAndDataType("z", "string", "4.50!"))
    assert(interpreter.expectVariableToExistWithValueAndDataType("a", "string", "1Hello1"))
  }

  describe("print statement") {
    it("arithmetic decimal") {
      val fileContent = Using(Source.fromURL(getClass.getResource("/printStatement/arithmetic-operations-decimal.ps"))) { source => source.mkString }
      val fileContentAsString = fileContent.getOrElse(throw new Exception("Could not find test file"))

      val lexer = new Lexer(fileContentAsString)
      val tokens = lexer.lex

      val parser = new Parser()
      val abstractSyntaxTree = parser.parse(fileContentAsString, tokens)

      interpreter.interpret(abstractSyntaxTree)
    }

    it("arithmetic") {
      val fileContent = Using(Source.fromURL(getClass.getResource("/printStatement/arithmetic-operations.ps"))) { source => source.mkString }
      val fileContentAsString = fileContent.getOrElse(throw new Exception("Could not find test file"))

      val lexer = new Lexer(fileContentAsString)
      val tokens = lexer.lex

      val parser = new Parser()
      val abstractSyntaxTree = parser.parse(fileContentAsString, tokens)

      interpreter.interpret(abstractSyntaxTree)
    }

    it("simple declaration and assignation") {
      val fileContent = Using(Source.fromURL(getClass.getResource("/printStatement/simple-declare-assign.ps"))) { source => source.mkString }
      val fileContentAsString = fileContent.getOrElse(throw new Exception("Could not find test file"))

      val lexer = new Lexer(fileContentAsString)
      val tokens = lexer.lex

      val parser = new Parser()
      val abstractSyntaxTree = parser.parse(fileContentAsString, tokens)

      interpreter.interpret(abstractSyntaxTree)
    }

    it("string and number concatenation") {
      val fileContent = Using(Source.fromURL(getClass.getResource("/printStatement/string-and-number-concat.ps"))) { source => source.mkString }
      val fileContentAsString = fileContent.getOrElse(throw new Exception("Could not find test file"))

      val lexer = new Lexer(fileContentAsString)
      val tokens = lexer.lex

      val parser = new Parser()
      val abstractSyntaxTree = parser.parse(fileContentAsString, tokens)

      interpreter.interpret(abstractSyntaxTree)
    }
  }

  describe("readInput statement") {
    it("should return a list of tokens of a readInput statement") {
      val fileContent = Using(Source.fromURL(getClass.getResource("/readInput.ps"))) { source => source.mkString }
      val fileContentAsString = fileContent.getOrElse(throw new Exception("Could not find test file"))

      val lexer = new Lexer(fileContentAsString)
      val tokens = lexer.lex

      val parser = new Parser()
      val abstractSyntaxTree = parser.parse(fileContentAsString, tokens)

      interpreter.interpret(abstractSyntaxTree)
    }
  }

  describe("if/else statement") {
    it("should interpret an if statement") {
      val fileContent = Using(Source.fromURL(getClass.getResource("/ifElse/if.ps"))) { source => source.mkString }
      val fileContentAsString = fileContent.getOrElse(throw new Exception("Could not find test file"))

      val lexer = new Lexer(fileContentAsString)
      val tokens = lexer.lex

      val parser = new Parser()
      val abstractSyntaxTree = parser.parse(fileContentAsString, tokens)

      interpreter.interpret(abstractSyntaxTree)
      assert(interpreter.expectVariableToExistWithValueAndDataType("bool", "boolean", "true"))
      assert(interpreter.expectVariableToExistWithValueAndDataType("num", "number", "1"))
    }

    it("should interpret an if/else statement") {
      val fileContent = Using(Source.fromURL(getClass.getResource("/ifElse/if-else.ps"))) { source => source.mkString }
      val fileContentAsString = fileContent.getOrElse(throw new Exception("Could not find test file"))

      val lexer = new Lexer(fileContentAsString)
      val tokens = lexer.lex

      val parser = new Parser()
      val abstractSyntaxTree = parser.parse(fileContentAsString, tokens)

      interpreter.interpret(abstractSyntaxTree)
      assert(interpreter.expectVariableToExistWithValueAndDataType("bool", "boolean", "false"))
      assert(interpreter.expectVariableToExistWithValueAndDataType("num", "number", "2"))
    }

    describe("operations") {
      it("should return a list of tokens of a math operation statement") {
        val fileContent = Using(Source.fromURL(getClass.getResource("/operation/math-operation.ps"))) { source => source.mkString }
        val fileContentAsString = fileContent.getOrElse(throw new Exception("Could not find test file"))

        val lexer = new Lexer(fileContentAsString)
        val tokens = lexer.lex

        val parser = new Parser()
        val abstractSyntaxTree = parser.parse(fileContentAsString, tokens)

        interpreter.interpret(abstractSyntaxTree)
        assert(interpreter.expectConstantToExistWithValueAndDataType("num", "number", "2.20"))
      }

      it("should return a list of tokens of a boolean operation statement") {
        val fileContent = Using(Source.fromURL(getClass.getResource("/operation/boolean-operation.ps"))) { source => source.mkString }
        val fileContentAsString = fileContent.getOrElse(throw new Exception("Could not find test file"))

        val lexer = new Lexer(fileContentAsString)
        val tokens = lexer.lex

        val parser = new Parser()
        val abstractSyntaxTree = parser.parse(fileContentAsString, tokens)

        interpreter.interpret(abstractSyntaxTree)
        assert(interpreter.expectConstantToExistWithValueAndDataType("bool", "boolean", "true"))
      }
    }
  }
}
