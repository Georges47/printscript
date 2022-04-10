package parser
import abstractSyntaxTree.{AbstractSyntaxTree, Node}
import token.TokenConsumerImpl
import token.types.{ClosedParenthesis, OpenParenthesis, Println, Semicolon}

/**
 *  Manages the parsing when a Println token type is found
 */
case class PrintlnHelper() extends ParserHelper {
  override def parse(tokenConsumer: TokenConsumerImpl): AbstractSyntaxTree = {
    tokenConsumer.consume(Println)
    tokenConsumer.consume(OpenParenthesis)
    val expression = ExpressionHelper().parse(tokenConsumer)
    tokenConsumer.consume(ClosedParenthesis)

    val currentToken = tokenConsumer.current
    currentToken.getType match {
      case Semicolon =>
        tokenConsumer.consume(Semicolon)
          AbstractSyntaxTree(
            Node("Println", Println),
            List(expression)
          )
      case _ =>
        throw new Exception(
          s"Expected semicolon at line ${currentToken.getRange.getEndLine}, column ${currentToken.getRange.getEndCol}"
        )
    }
  }
}
