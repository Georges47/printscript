package app

import interpreter.inputs.ConsoleInputProvider
import interpreter.{IdentifierTable, Interpreter}
import lexer.Lexer
import parser.Parser

import java.io.FileNotFoundException
import scala.io.Source

object CLI {
  val validOptions = List("printAST", "onlyParse", "")

  def start(): Unit = {
    printUsage()

    var input = scala.io.StdIn.readLine("> ")

    while (input != "q!") {
      var options: List[String] = List()
      var content = ""
      var error = ""

      if (input.contains("f!")) {
        options = input.split("f!").head.split(" ").toList
        val filePath = input.split("f!").tail.head.trim
        try {
          val bufferedSource = Source.fromFile(filePath)
          content = bufferedSource.mkString
        } catch {
          case _: FileNotFoundException => error = "Couldn't find file"
        }
      } else if (input.contains("t!")) {
        options = input.split("t!").head.split(" ").toList
        content = input.split("t!").tail.head.trim
      }

      options.foreach(option =>
        if (!validOptions.contains(option)) error = s"Unknown option: $option"
      )

      if (error.isEmpty) {
        val lexer = new Lexer(content)
        val tokens = lexer.lex

        val parser = new Parser()
        val ast = parser.parse(content, tokens)

        if (options.contains("printAST")) {
          println(ast)
        }

        if (!options.contains("onlyParse")) {
          val interpreter = new Interpreter
          interpreter.interpret(ast)
        }
      } else {
        println(error)
      }

      input = scala.io.StdIn.readLine("> ")
    }

    println("Goodbye!")
  }

  private def printUsage(): Unit = {
    print(
      "printscript 1.1 \n" +
        "Usage: [options] f! [<filePath>] or [options] t! [<text>] \n" +
        "Options: \n" +
        "\t printAST \t prints the complete abstract syntax tree \n" +
        "\t onlyParse \t only parses and validates the code \n" +
        "Exit: q! \n"
    )
  }
}
