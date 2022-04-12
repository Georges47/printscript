package parser
import abstractSyntaxTree.{AbstractSyntaxTree, Node}
import org.austral.ingsis.printscript.common.TokenType
import token.TokenConsumerImpl
import token.types.{Assignment, Colon, Declaration, DeclarationAndAssignment, Identifier, Let, Semicolon}

/**
 *  Manages the parsing when a Let token type is found
 */
case class LetHelper(dataTypes: List[TokenType]) extends ParserHelper {
  override def parse(tokenConsumer: TokenConsumerImpl): AbstractSyntaxTree = {
    tokenConsumer.consume(Let)
    val identifier = tokenConsumer.consume(Identifier)
    tokenConsumer.consume(Colon)
    if (!dataTypes.contains(tokenConsumer.current.getType)) {
      throw new Exception(s"Unknown data type in line ${tokenConsumer.current.getRange.getStartLine}, column ${tokenConsumer.current.getRange.getStartCol}")
    }

    val dataType = tokenConsumer.consumeAny(dataTypes:_*)
    val currentToken = tokenConsumer.current
    currentToken.getType match {
      case Assignment =>
        tokenConsumer.consume(Assignment)
        val expression = ExpressionHelper().parse(tokenConsumer)
        if (tokenConsumer.current.getType == Semicolon) tokenConsumer.consume(Semicolon)
        AbstractSyntaxTree(
          Node("DeclarationAndAssignment", DeclarationAndAssignment),
          List(
            AbstractSyntaxTree(Node.nodeFromContent(identifier)),
            AbstractSyntaxTree(Node.nodeFromContent(dataType)),
            expression
          )
        )
      case Semicolon =>
        tokenConsumer.consume(Semicolon)
        AbstractSyntaxTree(
          Node("Declaration", Declaration),
          List(
            AbstractSyntaxTree(Node.nodeFromContent(identifier)),
            AbstractSyntaxTree(Node.nodeFromContent(dataType))
          )
        )
      case _ =>
        throw new Exception(
          s"Expected semicolon at line ${currentToken.getRange.getEndLine}, column ${currentToken.getRange.getEndCol}"
        )
    }
  }
}
