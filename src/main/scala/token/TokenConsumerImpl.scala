package token

import org.austral.ingsis.printscript.common.{StringRead, TokenConsumer}
import org.austral.ingsis.printscript.parser.{Content, TokenIterator}

import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`

case class TokenConsumerImpl(tokenIterator: TokenIterator) extends TokenConsumer(tokenIterator) {
  def peek(amount: Int): List[Content[String]] = {
    tokenIterator.peek(amount, StringRead.INSTANCE).toList
  }
}
