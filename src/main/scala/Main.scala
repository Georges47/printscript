import lexer.Lexer
import parser.Parser

import scala.io.Source

object Main extends App {
  print("Enter the absolute path of the text file: ")
  val pathToFile = s"${System.getProperty("user.home")}/Documents/example" // StdIn.readLine()
  val bufferedSource = Source.fromFile(pathToFile)
  val fileContent: String = bufferedSource.mkString
  bufferedSource.close

  val lexer = new Lexer
  val tokens = lexer.getTokens(fileContent.toIterator)

  println("\n")
  tokens.foreach(println)
  println(" ")

  val parser = new Parser
  val ast = parser.parse(fileContent, tokens)

  println(ast)
}
