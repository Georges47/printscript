package app

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.{IdentifierTable, Interpreter}
import lexer.Lexer
import org.austral.ingsis.printscript.common.Token
import parser.Parser
import token.types.{Block, BooleanDataType, BooleanValue, ClosedBracket, Const, ConstantIdentifier, Else, If, OpenBracket}

import scala.collection.mutable.ListBuffer

class PrintScriptApp(version: String) {

  def lex(content: String): List[Token] = {
    val lexer = new Lexer(content)
    val tokens = lexer.lex
    println(version)
    println(version == "1.0")
    println(
      tokens
        .map(_.getType)
        .intersect(
          List(
            Const,
            If,
            Else,
            OpenBracket,
            ClosedBracket,
            ConstantIdentifier,
            BooleanValue,
            Block,
            BooleanDataType
          )
        )
        .nonEmpty
    )
    if (
      version == "1.0" && tokens
        .map(_.getType)
        .intersect(
          List(
            Const,
            If,
            Else,
            OpenBracket,
            ClosedBracket,
            ConstantIdentifier,
            BooleanValue,
            Block,
            BooleanDataType
          )
        )
        .nonEmpty
    ) {
      throw new Exception("Unknown token")
    }
    tokens
  }

  def parse(content: String): AbstractSyntaxTree = {
    val tokens = lex(content)
    val parser = new Parser()
    parser.parse(content, tokens)
  }

  def interpret(content: String): Unit = {
    val ast = parse(content)
    val interpreter = new Interpreter(IdentifierTable(), IdentifierTable())
    interpreter.interpret(ast)
  }

  def testInterpret(content: String): ListBuffer[String] = {
    val ast = parse(content)
    val interpreter = new Interpreter(IdentifierTable(), IdentifierTable(), testMode = true)
    interpreter.interpret(ast)
    interpreter.logs
  }

}
