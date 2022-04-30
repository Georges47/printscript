package parser.helpers

import abstractSyntaxTree.{AbstractSyntaxTree, Node}
import org.austral.ingsis.printscript.common.TokenType
import parser.Parser
import token.TokenConsumerImpl
import token.types._

case class ConstHelper() extends ParserHelper {
  val dataTypes: List[TokenType] = Parser.dataTypes

  /** Contains the necessary logic for parsing a specific type of token
    *
    * @param tokenConsumer from which tokens will be consumed
    * @return an AbstractSyntaxTree of the tokens consumed
    */
  override def parse(tokenConsumer: TokenConsumerImpl): AbstractSyntaxTree = {
    tokenConsumer.consume(Const)
    val identifier = tokenConsumer.consume(Identifier)
    tokenConsumer.consume(Colon)
    if (!dataTypes.contains(tokenConsumer.current.getType)) {
      throw new Exception(
        s"Unknown data type in line ${tokenConsumer.current.getRange.getStartLine}, column ${tokenConsumer.current.getRange.getStartCol}"
      )
    }

    val dataType = tokenConsumer.consumeAny(dataTypes: _*)
    val currentToken = tokenConsumer.current
    currentToken.getType match {
      case Assignment =>
        tokenConsumer.consume(Assignment)
        val expression = ExpressionHelper().parse(tokenConsumer)
//        if (tokenConsumer.current.getType == Semicolon)
//          tokenConsumer.consume(Semicolon)
        AbstractSyntaxTree(
          Node("DeclarationAndAssignment", DeclarationAndAssignment),
          List(
            AbstractSyntaxTree(Node(identifier.getContent, ConstantIdentifier)),
            AbstractSyntaxTree(Node.nodeFromContent(dataType)),
            expression
          )
        )
      case _ =>
        throw new Exception(
          s"Expected assignment operator at line ${currentToken.getRange.getEndLine}, column ${currentToken.getRange.getEndCol}"
        )
    }
  }
}
