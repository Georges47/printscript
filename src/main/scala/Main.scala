import lexer.Lexer
import parser.Parser
//import parser.Parser

import scala.io.Source

object Main extends App {
  print("Enter the absolute path of the text file: ")
//  val pathToFile = StdIn.readLine()
  val pathToFile = "/home/lucbar/Documents/example"
  val bufferedSource = Source.fromFile(pathToFile)

  val chars = bufferedSource.mkString.toList
  val lexer = new Lexer
  val tokens = lexer.getTokens(chars.toIterator.buffered)
  println(" ")
  tokens.foreach(t => {
    println(t)
//    println(t.lexicalRange)
  })
  //println(tokens)
  val parser = new Parser
  val ast = parser.parse(tokens.toIterator.buffered)
  println(ast)

  bufferedSource.close
}
