package parser
import abstractSyntaxTree.{AbstractSyntaxTree, Node}
import token.TokenConsumerImpl
import token.types.{And, Asterisk, BooleanValue, ClosedParenthesis, ConstantIdentifier, Expression, FrontSlash, Identifier, Minus, NumberValue, OpenParenthesis, Or, Plus, ReadInput, Semicolon, StringValue, VariableIdentifier}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
 *  Manages the parsing when an Expression token type is found
 */
case class ExpressionHelper() extends ParserHelper {
  override def parse(tokenConsumer: TokenConsumerImpl): AbstractSyntaxTree = {
    val abstractSyntaxTree: ListBuffer[AbstractSyntaxTree] = ListBuffer.empty
    helper(tokenConsumer, abstractSyntaxTree, 0, 0)
  }

  @tailrec
  private def helper(tokenConsumer: TokenConsumerImpl, currentASTs: ListBuffer[AbstractSyntaxTree], numberOfOpenParenthesis: Int, numberOfClosedParenthesis: Int): AbstractSyntaxTree = {
    val previousContent = tokenConsumer.consumeAny(NumberValue, StringValue, BooleanValue, Identifier, OpenParenthesis, ClosedParenthesis, Plus, Minus, Asterisk, FrontSlash, And, Or)
    currentASTs += AbstractSyntaxTree(Node.nodeFromContent(previousContent))
    val currentToken = tokenConsumer.current
    val nextToken = tokenConsumer.peek(2)(1).getToken

    if (currentToken.getType == ClosedParenthesis &&
      numberOfOpenParenthesis == numberOfClosedParenthesis &&
      (nextToken.getType != Plus || nextToken.getType != Minus || nextToken.getType != Asterisk || nextToken.getType != FrontSlash)) {
      return AbstractSyntaxTree(Node("Expression", Expression), currentASTs.toList)
    }

    currentToken.getType match {
      case NumberValue | StringValue | BooleanValue | Identifier => helper(tokenConsumer, currentASTs, numberOfOpenParenthesis, numberOfClosedParenthesis)
      case Plus | Minus | Asterisk | FrontSlash | And | Or => helper(tokenConsumer, currentASTs, numberOfOpenParenthesis, numberOfClosedParenthesis)
      case OpenParenthesis => helper(tokenConsumer, currentASTs, numberOfOpenParenthesis + 1, numberOfClosedParenthesis)
      case ClosedParenthesis => helper(tokenConsumer, currentASTs, numberOfOpenParenthesis, numberOfClosedParenthesis + 1)
      case Semicolon  =>
        tokenConsumer.consumeAny(Semicolon)
        AbstractSyntaxTree(Node("Expression", Expression), currentASTs.toList)
    }

  }

}
