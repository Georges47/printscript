package app

import interpreter.Interpreter
import lexer.Lexer
import parser.Parser
import token.types.{BooleanDataType, NumberDataType, StringDataType}

import scala.io.Source

object Main extends App {
  val argsMap = args.map(arg => arg.split("=", 2) match { case Array(e1, e2) => (e1, e2)}).toMap

  argsMap.foreach(println)

  val validOptions = List("filepath", "text", "parseAndValidate")
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

  println(ast)

  if (argsMap.contains("parseAndValidate") && argsMap("parseAndValidate") == "true") {
    println(ast)
  } else {
    val interpreter = new Interpreter
    interpreter.interpret(ast)
  }
}