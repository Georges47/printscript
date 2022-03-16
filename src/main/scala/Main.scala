import lexer.Lexer
import parser.Parser
//import parser.Parser

import scala.io.{Source, StdIn}

object Main extends App {
  print("Enter the absolute path of the text file: ")
//  val pathToFile = StdIn.readLine()
  val pathToFile = "/home/dimi/Documents/example.txt"
  val bufferedSource = Source.fromFile(pathToFile)

  val chars = bufferedSource.mkString.toList
  val lexer = new Lexer
  val tokens = lexer.getTokens(chars.toIterator.buffered)
  println(" ")
  //println(tokens)
  val parser = new Parser
  val ast = parser.getAbstractSyntaxTree(tokens.toIterator.buffered)
  println(ast)


  bufferedSource.close
}
