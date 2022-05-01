package token

import org.austral.ingsis.printscript.common.{StringRead, TokenConsumer}
import org.austral.ingsis.printscript.parser.{Content, TokenIterator}

import scala.jdk.CollectionConverters.ListHasAsScala

case class TokenConsumerImpl(tokenIterator: TokenIterator) extends TokenConsumer(tokenIterator) {

  /** Returns the contents of the next tokens (specified by the param) without consuming them
    *
    * @param amount number of token contents to return
    * @return content of the next tokens
    */
  def peek(amount: Int): List[Content[String]] = {
    tokenIterator.peek(amount, StringRead.INSTANCE).asScala.toList
  }
}
