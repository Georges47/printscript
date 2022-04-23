package parser

import abstractSyntaxTree.{AbstractSyntaxTree, Node}
import org.austral.ingsis.printscript.common.{Token, TokenType}
import org.austral.ingsis.printscript.parser.TokenIterator
import token.TokenConsumerImpl
import token.types._

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ListBuffer

// @param dataTypes all valid data types for variables
object Parser {
  val dataTypes = List(StringDataType, NumberDataType, BooleanDataType)
}

/**
 * Converts a list of Tokens into an AbstractSyntaxTree
 */
class Parser() {
  private val helpers = ParserHelper.helpers

  def parse(fileContent: String, tokens: List[Token]): AbstractSyntaxTree = {
    val tokenIterator = TokenIterator.create(fileContent, filterWhitespacesAndTabsAndNewlines(tokens).toBuffer.asJava)
    val tokenConsumer = TokenConsumerImpl(tokenIterator)
    parse(tokenConsumer, EndOfFile, Program)
  }

  def parse(tokenConsumer: TokenConsumerImpl, breakType: TokenType, rootType: TokenType = Program): AbstractSyntaxTree = {
    val abstractSyntaxTree: ListBuffer[AbstractSyntaxTree] = ListBuffer.empty
    while (tokenConsumer.current.getType != breakType) {
      val someAST = consumeTokens(tokenConsumer)
      abstractSyntaxTree += someAST
    }
    tokenConsumer.consume(breakType)
    abstractSyntaxTree += AbstractSyntaxTree(Node(breakType.toString, breakType))
    abstractSyntaxTree.toList
    AbstractSyntaxTree(Node(rootType.toString, rootType), abstractSyntaxTree.toList)
  }

  private def filterWhitespacesAndTabsAndNewlines(tokens: List[Token]): List[Token] = {
    tokens.filter(token => token.getType != Whitespace && token.getType != Tab && token.getType != Newline)
  }

  private def consumeTokens(tokenConsumer: TokenConsumerImpl): AbstractSyntaxTree = {
    val currentTokenType = tokenConsumer.current.getType

    if (ParserHelper.helpers.contains(currentTokenType)) {
      helpers(currentTokenType).parse(tokenConsumer)
    } else {
      // todo explota si hago ...=(...) <op> <value>;, llega aca por closed parentehsis
      throw new Exception(s"Unknown token of type $currentTokenType")
    }
  }

}
