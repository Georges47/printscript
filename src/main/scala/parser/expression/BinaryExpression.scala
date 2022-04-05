package parser.expression
import org.austral.ingsis.printscript.common.{LexicalRange, Token, TokenType}
import org.austral.ingsis.printscript.parser.Content
import token.types.{NumberValue, StringValue}

case class BinaryExpression(
    left: Expression,
    operator: Content[String],
    right: Expression
) extends Expression {
  override val content: Content[String] = calculateContent

  override def value: String = content.getContent

  override def expressionType: TokenType = content.getToken.getType

  def calculateContent: Content[String] = {
    operator.getContent match {
      case "+" => sum(left, right)
      case _ =>
        val content = left.value + right.value
        val token = new Token(
          NumberValue,
          left.content.getToken.getFrom,
          right.content.getToken.getTo,
          new LexicalRange(
            left.content.getToken.getRange.getStartCol,
            left.content.getToken.getRange.getStartLine,
            right.content.getToken.getRange.getEndCol,
            right.content.getToken.getRange.getEndLine
          )
        )
        new Content(content, token)
    }
  }

  def sum(left: Expression, right: Expression): Content[String] = {
    (left.expressionType, right.expressionType) match {
      case (NumberValue, NumberValue) =>
        val content = (left.value.toDouble + right.value.toDouble).toString
        val token = new Token(
          NumberValue,
          left.content.getToken.getFrom,
          right.content.getToken.getTo,
          new LexicalRange(
            left.content.getToken.getRange.getStartCol,
            left.content.getToken.getRange.getStartLine,
            right.content.getToken.getRange.getEndCol,
            right.content.getToken.getRange.getEndLine
          )
        )
        new Content(content, token)
      case (StringValue, StringValue) | (StringValue, NumberValue) |
          (NumberValue, StringValue) =>
        val content = "\"" + left.value.replaceAll("^\"|\"$", "") + right.value
          .replaceAll("^\"|\"$", "") + "\""
        val token = new Token(
          StringValue,
          left.content.getToken.getFrom,
          right.content.getToken.getTo,
          new LexicalRange(
            left.content.getToken.getRange.getStartCol,
            left.content.getToken.getRange.getStartLine,
            right.content.getToken.getRange.getEndCol,
            right.content.getToken.getRange.getEndLine
          )
        )
        new Content(content, token)
      case _ => // y si tengo un Identifier ?
        val content = s"${left.value} + ${right.value}"
        val token = new Token(
          StringValue,
          left.content.getToken.getFrom,
          right.content.getToken.getTo,
          new LexicalRange(
            left.content.getToken.getRange.getStartCol,
            left.content.getToken.getRange.getStartLine,
            right.content.getToken.getRange.getEndCol,
            right.content.getToken.getRange.getEndLine
          )
        )
        new Content(content, token)
    }
  }
}
