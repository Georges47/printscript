package parser
import abstractSyntaxTree.{AbstractSyntaxTree, Node}
import token.TokenConsumerImpl
import token.types._

/**
 *  Manages the parsing when an Identifier token type is found
 */
case class IdentifierHelper() extends ParserHelper {
  override def parse(tokenConsumer: TokenConsumerImpl): AbstractSyntaxTree = {
    val nextToken = tokenConsumer.peek(2)(1).getToken
    nextToken.getType match {
      case Assignment => parseAssignment(tokenConsumer)
      case Plus | Minus | Asterisk | FrontSlash | Semicolon => ExpressionHelper().parse(tokenConsumer)
    }
  }

  /**
   * Manages the parsing when an Assignment token type is found
   * @param tokenConsumer from which tokens will be consumed
   * @return an AbstractSyntaxTree of the tokens consumed
   */
  private def parseAssignment(tokenConsumer: TokenConsumerImpl): AbstractSyntaxTree = {
    val identifier = tokenConsumer.consume(Identifier)
    tokenConsumer.consume(Assignment)
    val expression = ExpressionHelper().parse(tokenConsumer)
    if (tokenConsumer.current.getType == Semicolon) tokenConsumer.consume(Semicolon)
    AbstractSyntaxTree(Node("Assignment", Assignment), List(AbstractSyntaxTree(Node.nodeFromContent(identifier)), expression))
  }
}
