package parser

import abstractSyntaxTree.AbstractSyntaxTree
import org.austral.ingsis.printscript.common.Token
import org.austral.ingsis.printscript.parser.TokenIterator
import parser.expression.{BinaryExpression, Literal}
import token.TokenConsumerImpl
import token.types._

import scala.collection.JavaConverters.seqAsJavaListConverter
import scala.collection.mutable.ListBuffer

class Parser {
  def parse(fileContent: String, tokens: List[Token]): AbstractSyntaxTree = {
    val abstractSyntaxTree: ListBuffer[AbstractSyntaxTree] = ListBuffer.empty
    val tokenIterator = TokenIterator.create(fileContent, tokens.asJava)
    val tokenConsumer = TokenConsumerImpl(tokenIterator)
    while (tokenConsumer.current.getType != EndOfFile) {
      abstractSyntaxTree += consumeTokens(tokenConsumer)
    }
    AbstractSyntaxTree("Program", abstractSyntaxTree.toList)
  }

  def consumeTokens(tokenConsumer: TokenConsumerImpl): AbstractSyntaxTree = {
    val currentToken = tokenConsumer.current
    currentToken.getType match {
      case Let     => parseVariableDeclarationAssignation(tokenConsumer)
      case Println => parsePrintln(tokenConsumer)
      case NumberValue | StringValue | Identifier | OpenParenthesis =>
        parseBinaryExpression(tokenConsumer)
      case Newline =>
        tokenConsumer.consume(currentToken.getType); AbstractSyntaxTree()
      case _ =>
        println(tokenConsumer.consume(currentToken.getType))
        AbstractSyntaxTree("<Unknown>")
    }
  }

  def parseVariableDeclarationAssignation(
      tokenConsumer: TokenConsumerImpl
  ): AbstractSyntaxTree = {
    tokenConsumer.consume(Let)
    val identifier = tokenConsumer.consume(Identifier)
    tokenConsumer.consume(Colon)
    val dataType = tokenConsumer.consumeAny(StringDataType, NumberDataType)

    val currentToken = tokenConsumer.current
    currentToken.getType match {
      case Assignment =>
        tokenConsumer.consume(Assignment)
        val expression = tokenConsumer.consumeAny( // Aqui deberia ser donde consume una expresion
          StringValue,
          NumberValue
        )
        tokenConsumer.consume(Semicolon)
        AbstractSyntaxTree(
          "DeclarationAndAssignation",
          List(
            AbstractSyntaxTree(identifier.getContent),
            AbstractSyntaxTree(dataType.getContent),
            AbstractSyntaxTree(expression.getContent)
          )
        )
      case Semicolon =>
        tokenConsumer.consume(Semicolon)
        AbstractSyntaxTree(
          "Declaration",
          List(
            AbstractSyntaxTree(identifier.getContent),
            AbstractSyntaxTree(dataType.getContent)
          )
        )
      case _ =>
        throw new Exception(
          s"Expected semicolon at line ${currentToken.getRange.getEndLine}, column ${currentToken.getRange.getEndCol}"
        )
    }
  }

  def parsePrintln(tokenConsumer: TokenConsumerImpl): AbstractSyntaxTree = {
    tokenConsumer.consume(Println)
    tokenConsumer.consume(OpenParenthesis)
    val expression = tokenConsumer.consumeAny(StringValue, NumberValue)
    tokenConsumer.consume(ClosedParenthesis)

    val currentToken = tokenConsumer.current
    currentToken.getType match {
      case Semicolon =>
        tokenConsumer.consume(Semicolon)
        AbstractSyntaxTree(
          "Println",
          List(AbstractSyntaxTree(expression.getContent))
        )
      case _ =>
        throw new Exception(
          s"Expected semicolon at line ${currentToken.getRange.getEndLine}, column ${currentToken.getRange.getEndCol}"
        )
    }
  }

  def parseBinaryExpression(
      tokenConsumer: TokenConsumerImpl
  ): AbstractSyntaxTree = {
    val leftValue =
      tokenConsumer.consumeAny(NumberValue, StringValue, Identifier)
    val operator = tokenConsumer.consumeAny(Minus, Plus, Asterisk, FrontSlash)
    val rightValue =
      tokenConsumer.consumeAny(NumberValue, StringValue, Identifier)

    val currentToken = tokenConsumer.current
    currentToken.getType match {
      case Semicolon =>
        tokenConsumer.consume(Semicolon)
        val expression =
          BinaryExpression(Literal(leftValue), operator, Literal(rightValue))
        AbstractSyntaxTree(
          expression.expressionType.toString,
          List(AbstractSyntaxTree(expression.value))
        )
      case _ =>
        throw new Exception(
          s"Expected semicolon at line ${currentToken.getRange.getEndLine}, column ${currentToken.getRange.getEndCol}"
        )
    }
  }

}
