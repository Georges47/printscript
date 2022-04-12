import scala.io.Source
import lexer.Lexer
import parser.Parser
import interpreter.Interpreter
import token.types.{NumberDataType, StringDataType}

object Main extends App {
    val bufferedSource = Source.fromFile(args(0))
    val fileContent: String = bufferedSource.mkString
    bufferedSource.close

    val lexer = new Lexer(fileContent)
    val tokens = lexer.lex

    val dataTypes = List(StringDataType, NumberDataType)
    val parser = new Parser(dataTypes)
    val ast = parser.parse(fileContent, tokens)

    val interpreter = new Interpreter
    interpreter.interpret(ast)

}
