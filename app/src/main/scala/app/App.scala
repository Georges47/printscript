package app

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.Interpreter
import lexer.Lexer
import org.austral.ingsis.printscript.common.Token
import parser.Parser

object App {
  def lex(content: String): List[Token] = {
    val lexer = new Lexer(content)
    lexer.lex
  }

  def parse(content: String): AbstractSyntaxTree = {
    val tokens = lex(content)
    val parser = new Parser()
    parser.parse(content, tokens)
  }

  def interpret(content: String): Unit = {
    val ast = parse(content)
    val interpreter = new Interpreter
    interpreter.interpret(ast)
  }
}
