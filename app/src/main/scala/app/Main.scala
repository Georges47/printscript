package app

import interpreter.Interpreter
import lexer.Lexer
import parser.Parser

import scala.io.Source

object Main extends App {
  val argsMap = args
    .map(arg => arg.split("=", 2) match { case Array(e1, e2) => (e1, e2) })
    .toMap

  val validOptions = List("filepath", "text", "onlyParseAndValidate", "printAST")
  argsMap.keySet.foreach(k => if (!validOptions.contains(k)) println(s"Unknown option: $k"))

  val content = if (argsMap.contains("filepath")) {
    val bufferedSource = Source.fromFile(argsMap("filepath"))
    bufferedSource.mkString
  } else {
    argsMap("text")
  }
  val lexer = new Lexer(content)
  val tokens = lexer.lex

  val parser = new Parser()
  val ast = parser.parse(content, tokens)

  if (argsMap.contains("printAST") && argsMap("printAST") == "true") {
    println(ast)
  }

  if (!(argsMap.contains("onlyParseAndValidate") && argsMap("onlyParseAndValidate") == "true")) {
    val interpreter = new Interpreter
    interpreter.interpret(ast)
  }

}
