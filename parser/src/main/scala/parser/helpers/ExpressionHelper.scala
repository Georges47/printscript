package parser.helpers

import abstractSyntaxTree.{AbstractSyntaxTree, Node}
import org.austral.ingsis.printscript.common.TokenType
import token.TokenConsumerImpl
import token.types._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/** Manages the parsing when an Expression token type is found
  */
case class ExpressionHelper() extends ParserHelper {
  override def parse(tokenConsumer: TokenConsumerImpl): AbstractSyntaxTree = {
    val abstractSyntaxTree: ListBuffer[AbstractSyntaxTree] = ListBuffer.empty
    if (tokenConsumer.current.getType == OpenParenthesis) {
      helper(tokenConsumer, abstractSyntaxTree, 1, 0)
    } else {
      helper(tokenConsumer, abstractSyntaxTree, 0, 0)
    }
  }

  @tailrec
  private def helper(
      tokenConsumer: TokenConsumerImpl,
      currentASTs: ListBuffer[AbstractSyntaxTree],
      numberOfOpenParenthesis: Int,
      numberOfClosedParenthesis: Int
  ): AbstractSyntaxTree = {
    val previousContent = tokenConsumer.consumeAny(
      NumberValue,
      StringValue,
      BooleanValue,
      Identifier,
      OpenParenthesis,
      ClosedParenthesis,
      Plus,
      Minus,
      Asterisk,
      FrontSlash,
      And,
      Or
    )

    currentASTs += AbstractSyntaxTree(Node.nodeFromContent(previousContent))
    val currentTokenType = tokenConsumer.current.getType
    val nextTokenType = tokenConsumer.peek(2)(1).getToken.getType

    if (finishedExpression(currentTokenType, numberOfOpenParenthesis, numberOfClosedParenthesis, nextTokenType)) {
      if (currentTokenType == Semicolon) tokenConsumer.consume(Semicolon)
      return AbstractSyntaxTree(Node("Expression", Expression), currentASTs.toList)
    }

    var openParenthesis = numberOfOpenParenthesis
    var closedParenthesis = numberOfClosedParenthesis

    if (currentTokenType == OpenParenthesis) {
      openParenthesis += 1
    } else if(currentTokenType == ClosedParenthesis) {
      closedParenthesis += 1
    }

    helper(
      tokenConsumer,
      currentASTs,
      openParenthesis,
      closedParenthesis
    )
  }

  private def finishedExpression(currentTokenType: TokenType, numberOfOpenParenthesis: Int, numberOfClosedParenthesis: Int, nextTokenType: TokenType): Boolean = {
    (currentTokenType == ClosedParenthesis &&
    numberOfOpenParenthesis == numberOfClosedParenthesis &&
    (nextTokenType != Plus || nextTokenType != Minus || nextTokenType != Asterisk || nextTokenType != FrontSlash)) ||
    currentTokenType == Semicolon
  }

}
