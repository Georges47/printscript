package interpreter

import interpreter.inputs.ConsoleInputProvider
import lexer.Lexer
import org.scalatest.funspec.AnyFunSpec
import parser.Parser

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
    it("should interpret a variable assignment with readInput") {
      val fileContent = Using(Source.fromURL(getClass.getResource("/readInput/variable-assignment.ps"))) { source => source.mkString }
      val fileContentAsString = fileContent.getOrElse(throw new Exception("Could not find test file"))

      val lexer = new Lexer(fileContentAsString)
      val tokens = lexer.lex

      val parser = new Parser()
      val abstractSyntaxTree = parser.parse(fileContentAsString, tokens)

      interpreter.interpret(abstractSyntaxTree)
    }

    it("should interpret a readInput statement") {
      val fileContent = Using(Source.fromURL(getClass.getResource("/readInput/read-input-statement.ps"))) { source => source.mkString }
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
      assert(interpreter.expectVariableToExistWithValueAndDataType("num", "number", "2"))
      assert(interpreter.expectVariableToExistWithValueAndDataType("str", "string", "\"1\""))
    }

    describe("operations") {
      it("should interpret a math operation statement") {
        val fileContent = Using(Source.fromURL(getClass.getResource("/operation/math-operation.ps"))) { source => source.mkString }
        val fileContentAsString = fileContent.getOrElse(throw new Exception("Could not find test file"))

        val lexer = new Lexer(fileContentAsString)
        val tokens = lexer.lex

        val parser = new Parser()
        val abstractSyntaxTree = parser.parse(fileContentAsString, tokens)

        interpreter.interpret(abstractSyntaxTree)
        assert(interpreter.expectConstantToExistWithValueAndDataType("num", "number", "2.20"))
      }

      it("should interpret a boolean operation statement") {
        val fileContent = Using(Source.fromURL(getClass.getResource("/operation/boolean-operation.ps"))) { source => source.mkString }
        val fileContentAsString = fileContent.getOrElse(throw new Exception("Could not find test file"))

        val lexer = new Lexer(fileContentAsString)
        val tokens = lexer.lex

        val parser = new Parser()
        val abstractSyntaxTree = parser.parse(fileContentAsString, tokens)

        interpreter.interpret(abstractSyntaxTree)
        assert(interpreter.expectConstantToExistWithValueAndDataType("bool", "boolean", "true"))
      }

      it("should interpret a math expression statement") {
        val fileContent = Using(Source.fromURL(getClass.getResource("/operation/math-expression.ps"))) { source => source.mkString }
        val fileContentAsString = fileContent.getOrElse(throw new Exception("Could not find test file"))

        val lexer = new Lexer(fileContentAsString)
        val tokens = lexer.lex

        val parser = new Parser()
        val abstractSyntaxTree = parser.parse(fileContentAsString, tokens)

        interpreter.interpret(abstractSyntaxTree)
        assert(interpreter.expectConstantToExistWithValueAndDataType("num", "number", "2"))
      }
    }

    describe("errors") {
      it("should throw an exception given a subtraction between strings") {
        val fileContent = Using(Source.fromURL(getClass.getResource("/operation/string-subtract-error.ps"))) { source => source.mkString }
        val fileContentAsString = fileContent.getOrElse(throw new Exception("Could not find test file"))

        val lexer = new Lexer(fileContentAsString)
        val tokens = lexer.lex

        val parser = new Parser()
        val abstractSyntaxTree = parser.parse(fileContentAsString, tokens)

        val thrown = intercept[Exception] {
          interpreter.interpret(abstractSyntaxTree)
        }
        assert(thrown.getMessage === "Invalid operator applied to string value")
      }

      it("should throw an exception given a multiplication between strings") {
        val fileContent = Using(Source.fromURL(getClass.getResource("/operation/string-multiplication-error.ps"))) { source => source.mkString }
        val fileContentAsString = fileContent.getOrElse(throw new Exception("Could not find test file"))

        val lexer = new Lexer(fileContentAsString)
        val tokens = lexer.lex

        val parser = new Parser()
        val abstractSyntaxTree = parser.parse(fileContentAsString, tokens)

        val thrown = intercept[Exception] {
          interpreter.interpret(abstractSyntaxTree)
        }
        assert(thrown.getMessage === "Invalid operator applied to string value")
      }

      it("should throw an exception given a division between strings") {
        val fileContent = Using(Source.fromURL(getClass.getResource("/operation/string-divide-error.ps"))) { source => source.mkString }
        val fileContentAsString = fileContent.getOrElse(throw new Exception("Could not find test file"))

        val lexer = new Lexer(fileContentAsString)
        val tokens = lexer.lex

        val parser = new Parser()
        val abstractSyntaxTree = parser.parse(fileContentAsString, tokens)

        val thrown = intercept[Exception] {
          interpreter.interpret(abstractSyntaxTree)
        }
        assert(thrown.getMessage === "Invalid operator applied to string value")
      }

      it("should throw an exception given a constant assignment") {
        val fileContent = Using(Source.fromURL(getClass.getResource("/reassigning-const-error.ps"))) { source => source.mkString }
        val fileContentAsString = fileContent.getOrElse(throw new Exception("Could not find test file"))

        val lexer = new Lexer(fileContentAsString)
        val tokens = lexer.lex

        val parser = new Parser()
        val abstractSyntaxTree = parser.parse(fileContentAsString, tokens)

        val thrown = intercept[Exception] {
          new Interpreter().interpret(abstractSyntaxTree)
        }
        assert(thrown.getMessage === "Reassigning constant at line 2, column 1")
      }

      it("should throw an exception given a non existing identifier") {
        val fileContent = Using(Source.fromURL(getClass.getResource("/non-existing-identifier-error.ps"))) { source => source.mkString }
        val fileContentAsString = fileContent.getOrElse(throw new Exception("Could not find test file"))

        val lexer = new Lexer(fileContentAsString)
        val tokens = lexer.lex

        val parser = new Parser()
        val abstractSyntaxTree = parser.parse(fileContentAsString, tokens)

        val thrown = intercept[Exception] {
          new Interpreter().interpret(abstractSyntaxTree)
        }
        assert(thrown.getMessage === "Non existing identifier at line 1, column 1")
      }
    }
  }
}
