package parser

import abstractSyntaxTree.{AbstractSyntaxTree, Node}
import org.austral.ingsis.printscript.common.{Token, TokenType}
import org.austral.ingsis.printscript.parser.TokenIterator
import token.TokenConsumerImpl
import token.types._

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ListBuffer

/**
 * Converts a list of Tokens into an AbstractSyntaxTree
 * @param dataTypes all valid data types for variables
 */
class Parser(dataTypes: List[TokenType]) {
  def parse(fileContent: String, tokens: List[Token]): AbstractSyntaxTree = {
//    val abstractSyntaxTree: ListBuffer[AbstractSyntaxTree] = ListBuffer.empty
    val tokenIterator = TokenIterator.create(fileContent, tokens.toBuffer.asJava)
    val tokenConsumer = TokenConsumerImpl(tokenIterator)
//    while (tokenConsumer.current.getType != EndOfFile) {
//      val someAST = Some(consumeTokens(tokenConsumer))
//      if(someAST.isDefined) abstractSyntaxTree += someAST.get
//    }
    AbstractSyntaxTree(Node("Program", Program), parseHelper(tokenConsumer))
  }

  def parseHelper(tokenConsumer: TokenConsumerImpl): List[AbstractSyntaxTree] = {
    val abstractSyntaxTree: ListBuffer[AbstractSyntaxTree] = ListBuffer.empty
    while (tokenConsumer.current.getType != EndOfFile && tokenConsumer.current.getType != ClosedBracket) {
      val someAST = consumeTokens(tokenConsumer)
      if (someAST.isDefined) abstractSyntaxTree += someAST.get
    }
    //    if(tokenConsumer.current.getType == ClosedBracket) tokenConsumer.consume(ClosedBracket)
    val currentToken = tokenConsumer.current
    if (currentToken.getType == EndOfFile || currentToken.getType == ClosedBracket) {
      abstractSyntaxTree += AbstractSyntaxTree(Node(currentToken.getType.getType, currentToken.getType))
      tokenConsumer.consume(currentToken.getType)
    }
    abstractSyntaxTree.toList
  }

  def consumeTokens(tokenConsumer: TokenConsumerImpl): Option[AbstractSyntaxTree] = {
    val currentToken = tokenConsumer.current
    currentToken.getType match {
      case Const => Some(ConstHelper(dataTypes).parse(tokenConsumer))
      case Let => Some(LetHelper(dataTypes).parse(tokenConsumer))
      case Identifier => Some(IdentifierHelper().parse(tokenConsumer))
      case Println => Some(PrintlnHelper().parse(tokenConsumer))
      case ReadInput =>
        tokenConsumer.consume(ReadInput)
        tokenConsumer.consume(OpenParenthesis)
        val messageToken = tokenConsumer.consumeAny(StringValue, Identifier)
        tokenConsumer.consume(ClosedParenthesis)

        val currentToken = tokenConsumer.current
        currentToken.getType match {
          case Semicolon =>
            tokenConsumer.consume(Semicolon)
            Some(
              AbstractSyntaxTree(
                Node("ReadInput", ReadInput),
                List(AbstractSyntaxTree(Node.nodeFromContent(messageToken)))
              )
            )
          case _ =>
            throw new Exception(
              s"Expected semicolon at line ${currentToken.getRange.getEndLine}, column ${currentToken.getRange.getEndCol}"
            )
        }

      case NumberValue | StringValue | OpenParenthesis =>
        Some(ExpressionHelper().parse(tokenConsumer))
      case Newline | ClosedBracket =>
        tokenConsumer.consume(currentToken.getType)
        None
      case If =>
        tokenConsumer.consume(If)
        tokenConsumer.consume(OpenParenthesis)
        val value = tokenConsumer.consumeAny(BooleanValue, Identifier)
        tokenConsumer.consume(ClosedParenthesis)
        tokenConsumer.consume(OpenBracket)

        val ifBlock = parseHelper(tokenConsumer)
        val nodes = ListBuffer(
          AbstractSyntaxTree(Node(value.getContent, value.getToken.getType)),
          AbstractSyntaxTree(Node("Block", Block), ifBlock)
        )

        if(tokenConsumer.current.getType == Else) {
          tokenConsumer.consume(Else)
          tokenConsumer.consume(OpenBracket)
          val elseBlock = parseHelper(tokenConsumer)
          nodes += AbstractSyntaxTree(Node("Block", Block), elseBlock)
        }

        val result = AbstractSyntaxTree(Node("If", If), nodes.toList)

        Some(result)
      case EndOfFile => Some(AbstractSyntaxTree(Node("EndOfFile", EndOfFile)))
      case _ =>
        // todo explota si hago ...=(...) <op> <value>;, llega aca por closed parentehsis
        tokenConsumer.consume(currentToken.getType)
        throw new Exception(s"Unknown token of type ${currentToken.getType}")
    }
  }

}
