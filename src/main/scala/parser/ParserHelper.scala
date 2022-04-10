package parser

import abstractSyntaxTree.AbstractSyntaxTree
import token.TokenConsumerImpl

/**
 *  Manages the parsing of a specific token type
 */
trait ParserHelper {
  /** Contains the necessary logic for parsing a specific type of token
   *
   * @param tokenConsumer from which tokens will be consumed
   * @return an AbstractSyntaxTree of the tokens consumed
   */
  def parse(tokenConsumer: TokenConsumerImpl): AbstractSyntaxTree
}
