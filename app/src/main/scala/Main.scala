import abstractSyntaxTree.AbstractSyntaxTree

import scala.io.Source
import lexer.Lexer
import parser.Parser
import interpreter.Interpreter
import token.types.{BooleanDataType, NumberDataType, StringDataType}

object Main extends App {
    val bufferedSource = if (args.isEmpty) {
        Source.fromFile(s"${System.getProperty("user.home")}/Documents/example.txt")
    } else {
         Source.fromFile(args(0))
    }
    val fileContent: String = bufferedSource.mkString
    bufferedSource.close

    val lexer = new Lexer(fileContent)
    val tokens = lexer.lex
    tokens.foreach(println)

    val dataTypes = List(StringDataType, NumberDataType, BooleanDataType)
    val parser = new Parser(dataTypes)
    val ast = parser.parse(fileContent, tokens)

    println(ast)
//    helper(ast)

    val interpreter = new Interpreter
    interpreter.interpret(ast)

    def helper(ast: AbstractSyntaxTree): Unit = {
        println(ast.root.tokenType)
        ast.nodes.foreach(node => helper(node))
    }
}