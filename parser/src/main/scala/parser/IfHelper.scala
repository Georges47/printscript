package parser
import abstractSyntaxTree.{AbstractSyntaxTree, Node}
import token.TokenConsumerImpl
import token.types.{Block, BooleanValue, ClosedBracket, ClosedParenthesis, Else, Identifier, If, OpenBracket, OpenParenthesis}

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
    if(tokenConsumer.current.getType == Else) {
      tokenConsumer.consume(Else)
      tokenConsumer.consume(OpenBracket)
      val elseBlock = new Parser().parse(tokenConsumer, ClosedBracket, Block)
      nodes += elseBlock
    }
    val result = AbstractSyntaxTree(Node("If", If), nodes.toList)
    result
  }

//  def parseHelper(tokenConsumer: TokenConsumerImpl): List[AbstractSyntaxTree] = {
//    val abstractSyntaxTree: ListBuffer[AbstractSyntaxTree] = ListBuffer.empty
//    while (tokenConsumer.current.getType != EndOfFile && tokenConsumer.current.getType != ClosedBracket) {
//      val someAST = consumeTokens(tokenConsumer)
//      if (someAST.isDefined) abstractSyntaxTree += someAST.get
//    }
//    val currentToken = tokenConsumer.current
//    if (currentToken.getType == EndOfFile || currentToken.getType == ClosedBracket) {
//      abstractSyntaxTree += AbstractSyntaxTree(Node(currentToken.getType.getType, currentToken.getType))
//      tokenConsumer.consume(currentToken.getType)
//    }
//    println("--------")
//    println(abstractSyntaxTree.toList.size)
//    println(abstractSyntaxTree)
//    println("--------\n")
//    abstractSyntaxTree.toList
//  }
}
