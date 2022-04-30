package parser.helpers

import abstractSyntaxTree.{AbstractSyntaxTree, Node}
import parser.Parser
import token.TokenConsumerImpl
import token.types._

import scala.collection.mutable.ListBuffer

case class IfHelper() extends ParserHelper {

  /** Contains the necessary logic for parsing a specific type of token
    *
    * @param tokenConsumer from which tokens will be consumed
    * @return an AbstractSyntaxTree of the tokens consumed
    */
  override def parse(tokenConsumer: TokenConsumerImpl): AbstractSyntaxTree = {
    tokenConsumer.consume(If)
    tokenConsumer.consume(OpenParenthesis)
    val value = tokenConsumer.consumeAny(BooleanValue, Identifier)
    tokenConsumer.consume(ClosedParenthesis)
    tokenConsumer.consume(OpenBracket)
    val ifBlock = new Parser().parse(tokenConsumer, ClosedBracket, Block)
    val nodes = ListBuffer(
      AbstractSyntaxTree(Node(value.getContent, value.getToken.getType)),
      ifBlock
    )
    if (tokenConsumer.current.getType == Else) {
      tokenConsumer.consume(Else)
      tokenConsumer.consume(OpenBracket)
      val elseBlock = new Parser().parse(tokenConsumer, ClosedBracket, Block)
      nodes += elseBlock
    }
    if (tokenConsumer.current.getType == Semicolon) tokenConsumer.consume(Semicolon)
    AbstractSyntaxTree(Node("If", If), nodes.toList)
  }
}
