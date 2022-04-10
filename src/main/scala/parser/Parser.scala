package parser

import abstractSyntaxTree.{AbstractSyntaxTree, Node}
import org.austral.ingsis.printscript.common.{Token, TokenType}
import org.austral.ingsis.printscript.parser.TokenIterator
import token.TokenConsumerImpl
import token.types._

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ListBuffer

/**
 * Converts a list of Tokens into an AbstractSyntaxTree
 * @param dataTypes all valid data types for variables
 */
class Parser(dataTypes: List[TokenType]) {
  def parse(fileContent: String, tokens: List[Token]): AbstractSyntaxTree = {
    val abstractSyntaxTree: ListBuffer[AbstractSyntaxTree] = ListBuffer.empty
    val tokenIterator = TokenIterator.create(fileContent, tokens.toBuffer.asJava)
    val tokenConsumer = TokenConsumerImpl(tokenIterator)
    while (tokenConsumer.current.getType != EndOfFile) {
      val someAST = Some(consumeTokens(tokenConsumer))
      if(someAST.isDefined) abstractSyntaxTree += someAST.get
      print("Current: ")
      println(tokenConsumer.current)
    }
    AbstractSyntaxTree(Node("Program", Program), abstractSyntaxTree.toList)
  }

  def consumeTokens(tokenConsumer: TokenConsumerImpl): AbstractSyntaxTree = {
    val currentToken = tokenConsumer.current
    println(currentToken)
    currentToken.getType match {
      case Let => LetHelper(dataTypes).parse(tokenConsumer)
      case Identifier => IdentifierHelper().parse(tokenConsumer)
      case Println => PrintlnHelper().parse(tokenConsumer)
      case NumberValue | StringValue | OpenParenthesis =>
        ExpressionHelper().parse(tokenConsumer)
      case Newline =>
        tokenConsumer.consume(currentToken.getType)
        consumeTokens(tokenConsumer)
      case EndOfFile => AbstractSyntaxTree(Node("EndOfFile", EndOfFile), List.empty)
      case _ =>
        tokenConsumer.consume(currentToken.getType)
        throw new Exception("Unknown token")
    }
  }

}
