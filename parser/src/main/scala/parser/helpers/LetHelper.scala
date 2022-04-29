package parser.helpers

import abstractSyntaxTree.{AbstractSyntaxTree, Node}
import org.austral.ingsis.printscript.common.TokenType
import parser.Parser
import token.TokenConsumerImpl
import token.types._

/** Manages the parsing when a Let token type is found */
case class LetHelper() extends ParserHelper {
  val dataTypes: List[TokenType] = Parser.dataTypes

  override def parse(tokenConsumer: TokenConsumerImpl): AbstractSyntaxTree = {
    tokenConsumer.consume(Let)
    val identifier = tokenConsumer.consume(Identifier)
    tokenConsumer.consume(Colon)

    if (dataTypeIsInvalid(tokenConsumer)) {
      val currentRange = tokenConsumer.current.getRange
      throw new Exception(
        s"Unknown data type in line ${currentRange.getStartLine}, column ${currentRange.getStartCol}"
      )
    }

    val dataType = tokenConsumer.consumeAny(dataTypes: _*)
    val currentTokenType = tokenConsumer.current.getType
    currentTokenType match {
      case Assignment =>
        tokenConsumer.consume(Assignment)

        val list = if (tokenConsumer.current.getType == ReadInput) {
          tokenConsumer.consume(ReadInput)
          tokenConsumer.consume(OpenParenthesis)
          val message = tokenConsumer.consume(StringValue)
          tokenConsumer.consume(ClosedParenthesis)
          List(
            AbstractSyntaxTree(Node(identifier.getContent, VariableIdentifier)),
            AbstractSyntaxTree(Node.nodeFromContent(dataType)),
            AbstractSyntaxTree(Node("ReadInput", ReadInput)),
            AbstractSyntaxTree(Node.nodeFromContent(message))
          )
        } else {
          val expression = ExpressionHelper().parse(tokenConsumer)
          List(
            AbstractSyntaxTree(Node(identifier.getContent, VariableIdentifier)),
            AbstractSyntaxTree(Node.nodeFromContent(dataType)),
            expression
          )
        }
        if (tokenConsumer.current.getType == Semicolon)
          tokenConsumer.consume(Semicolon)

        AbstractSyntaxTree(
          Node("DeclarationAndAssignment", DeclarationAndAssignment),
          list
        )
      case Semicolon =>
        tokenConsumer.consume(Semicolon)
        AbstractSyntaxTree(
          Node("Declaration", Declaration),
          List(
            AbstractSyntaxTree(Node(identifier.getContent, VariableIdentifier)),
            AbstractSyntaxTree(Node.nodeFromContent(dataType))
          )
        )
      case _ =>
        val currentRange = tokenConsumer.current.getRange
        throw new Exception(
          s"Expected semicolon at line ${currentRange.getEndLine}, column ${currentRange.getEndCol}"
        )
    }
  }

  private def dataTypeIsInvalid(tokenConsumer: TokenConsumerImpl): Boolean = {
    val currentTokenType = tokenConsumer.current.getType
    !dataTypes.contains(currentTokenType)
  }
}
