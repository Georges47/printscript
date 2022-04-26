package parser

import abstractSyntaxTree.{AbstractSyntaxTree, Node}
import org.austral.ingsis.printscript.common.{Token, TokenType}
import org.austral.ingsis.printscript.parser.TokenIterator
import parser.helpers.ParserHelper
import token.TokenConsumerImpl
import token.types._

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ListBuffer

object Parser {
  val dataTypes = List(StringDataType, NumberDataType, BooleanDataType)
}

/** Converts a list of Tokens into an AbstractSyntaxTree */
class Parser() {
  private val helpers = ParserHelper.helpers

  def parse(fileContent: String, tokens: List[Token]): AbstractSyntaxTree = {
    val tokenIterator = TokenIterator.create(
      fileContent,
      filter(tokens).toBuffer.asJava
    )
    val tokenConsumer = TokenConsumerImpl(tokenIterator)
    parse(tokenConsumer, EndOfFile, Program)
  }

  def parse(
      tokenConsumer: TokenConsumerImpl,
      breakType: TokenType,
      rootType: TokenType = Program
  ): AbstractSyntaxTree = {
    val astListBuffer: ListBuffer[AbstractSyntaxTree] = ListBuffer.empty
    while (tokenConsumer.current.getType != breakType) {
      astListBuffer += consumeTokens(tokenConsumer)
    }
    tokenConsumer.consume(breakType)
    astListBuffer += AbstractSyntaxTree(Node(breakType.toString, breakType))
    AbstractSyntaxTree(
      Node(rootType.toString, rootType),
      astListBuffer.toList
    )
  }

  private def filter(tokens: List[Token]): List[Token] = {
    val blacklist = List(Whitespace, Tab, Newline)
    tokens.filter(token => !blacklist.contains(token.getType))
  }

  private def consumeTokens(tokenConsumer: TokenConsumerImpl): AbstractSyntaxTree = {
    val currentTokenType = tokenConsumer.current.getType
    if (ParserHelper.helpers.contains(currentTokenType)) {
      helpers(currentTokenType).parse(tokenConsumer)
    } else {
      val currentRange = tokenConsumer.current.getRange
      throw new Exception(
        s"Unknown token of type $currentTokenType at line ${currentRange.getStartLine}, column ${currentRange.getStartCol}"
      )
    }
  }

}
