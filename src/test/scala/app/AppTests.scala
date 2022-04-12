//package app
//
//import lexer.Lexer
//import org.scalatest.funspec.AnyFunSpec
//import parser.Parser
//
//import scala.io.Source
//import scala.util.Using
//
//class AppTests extends AnyFunSpec {
//  it("should interpret a file") {
//    val fileContent = Using(Source.fromURL(getClass.getResource("/file"))) { source => source.mkString } // Source.fromURL(getClass.getResource("/file")).toString
//    val fileContentAsString = fileContent.getOrElse(throw new Exception("Could not find test file"))
//
//    val lexer = new Lexer(fileContentAsString)
//    val tokens = lexer.lex
//
//    val parser = new Parser(List(StringDataType, NumberDataType))
//    val abstractSyntaxTree = parser.parse(fileContentAsString, tokens)
//
//    val interpreter = new Interpreter
//    interpreter.interpret(abstractSyntaxTree)
//
//    assert(interpreter.expectVariableToExistWithValueAndDataType("x", "Number", "0.5"))
//    assert(interpreter.expectVariableToExistWithValueAndDataType("y", "Number", "4.5"))
//    assert(interpreter.expectVariableToExistWithValueAndDataType("z", "String", "5.0!"))
//    assert(interpreter.expectVariableToExistWithValueAndDataType("a", "String", "1Hello1"))
//  }
//}
