package app

import interpreter.Interpreter
import lexer.Lexer
import org.scalatest.funspec.AnyFunSpec
import parser.Parser
import token.types.{BooleanDataType, NumberDataType, StringDataType}

import scala.io.Source
import scala.util.Using

class AppTests extends AnyFunSpec {
  val interpreter = new Interpreter

  it("should interpret a file") {
    val fileContent = Using(Source.fromURL(getClass.getResource("/file"))) { source => source.mkString } // Source.fromURL(getClass.getResource("/file")).toString
    val fileContentAsString = fileContent.getOrElse(throw new Exception("Could not find test file"))

    val lexer = new Lexer(fileContentAsString)
    val tokens = lexer.lex

    val parser = new Parser()
    val abstractSyntaxTree = parser.parse(fileContentAsString, tokens)

    interpreter.interpret(abstractSyntaxTree)

    assert(interpreter.expectVariableToExistWithValueAndDataType("x", "number", "0.5"))
    assert(interpreter.expectVariableToExistWithValueAndDataType("y", "number", "4.5"))
    assert(interpreter.expectVariableToExistWithValueAndDataType("z", "string", "4.5!"))
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
}
