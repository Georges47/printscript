import scala.io.Source
import lexer.Lexer
import parser.Parser
import interpreter.Interpreter
import token.types.{NumberDataType, StringDataType}

object Main extends App {
  print("Enter the absolute path of the text file: ")
  val pathToFile =
    s"${System.getProperty("user.home")}/Documents/example.txt" // StdIn.readLine()
  println("\n")
  val bufferedSource = Source.fromFile(pathToFile)
  val fileContent: String = bufferedSource.mkString
  bufferedSource.close

  val lexer = new Lexer(fileContent)
  val tokens = lexer.lex

  tokens.foreach(println)

  val dataTypes = List(StringDataType, NumberDataType)
  val parser = new Parser(dataTypes)
  val ast = parser.parse(fileContent, tokens)

  println(ast)

  val interpreter = new Interpreter
  interpreter.interpret(ast)
}
