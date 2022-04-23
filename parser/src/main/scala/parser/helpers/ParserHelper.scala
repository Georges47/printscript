package parser.helpers

import abstractSyntaxTree.AbstractSyntaxTree
import org.austral.ingsis.printscript.common.TokenType
import token.TokenConsumerImpl
import token.types._

object ParserHelper {
  val helpers: Map[TokenType, ParserHelper] = Map(
    Const -> ConstHelper(),
    Let -> LetHelper(),
    If -> IfHelper(),
    Identifier -> IdentifierHelper(),
    Println -> PrintlnHelper(),
    ReadInput -> ReadInputHelper(),
    NumberValue -> ExpressionHelper(),
    StringValue -> ExpressionHelper(),
    OpenParenthesis -> ExpressionHelper()
  )
}

/**  Manages the parsing of a specific token type
  */
trait ParserHelper {

  /** Contains the necessary logic for parsing a specific type of token
    *
    * @param tokenConsumer from which tokens will be consumed
    * @return an AbstractSyntaxTree of the tokens consumed
    */
  def parse(tokenConsumer: TokenConsumerImpl): AbstractSyntaxTree
}
