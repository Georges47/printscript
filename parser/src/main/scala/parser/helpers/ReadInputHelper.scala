package parser.helpers

import abstractSyntaxTree.{AbstractSyntaxTree, Node}
import token.TokenConsumerImpl
import token.types._

case class ReadInputHelper() extends ParserHelper {

  /** Contains the necessary logic for parsing a specific type of token
    *
    * @param tokenConsumer from which tokens will be consumed
    * @return an AbstractSyntaxTree of the tokens consumed
    */
  override def parse(tokenConsumer: TokenConsumerImpl): AbstractSyntaxTree = {
    tokenConsumer.consume(ReadInput)
    tokenConsumer.consume(OpenParenthesis)
    val messageToken = tokenConsumer.consumeAny(StringValue, Identifier)
    tokenConsumer.consume(ClosedParenthesis)
    val currentToken = tokenConsumer.current
    currentToken.getType match {
      case Semicolon =>
        tokenConsumer.consume(Semicolon)
        AbstractSyntaxTree(
          Node("ReadInput", ReadInput),
          List(AbstractSyntaxTree(Node.nodeFromContent(messageToken)))
        )
      case _ =>
        throw new Exception(
          s"Expected semicolon at line ${currentToken.getRange.getEndLine}, column ${currentToken.getRange.getEndCol}"
        )
    }
  }
}
