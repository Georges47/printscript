package interpreter

import abstractSyntaxTree.AbstractSyntaxTree
import org.austral.ingsis.printscript.common.{LexicalRange, Token, TokenType}
import org.austral.ingsis.printscript.parser.Content
import token.types._

import scala.collection.mutable

case class ExpressionCalculator(variables: Map[String, (Option[String], String)]) {
  val values: mutable.Stack[Content[String]] = mutable.Stack().empty
  val operators: mutable.Stack[TokenType] = mutable.Stack().empty

  /** Applies the operator to the two corresponding values
   *
   * @param operator
   * @param leftContent
   * @param rightContent
   * @return the result of the calculation
   */
  def applyOperator(operator: TokenType, leftContent: Content[String], rightContent: Content[String]): Content[String] = {
    val leftValue = leftContent.getContent
    val rightValue = rightContent.getContent
    operator match {
      case Plus =>
        if (leftContent.getToken.getType == StringValue || rightContent.getToken.getType == StringValue)
          new Content(leftValue.replaceAll("^\"|\"$", "") + rightValue.replaceAll("^\"|\"$", ""), new Token(StringValue, 0, 0, new LexicalRange(0, 0, 0, 0)))
        else
          new Content((leftValue.toDouble + rightValue.toDouble).toString, new Token(NumberValue, 0, 0, new LexicalRange(0, 0, 0, 0)))
      case Minus =>
        if (leftContent.getToken.getType == StringValue || rightContent.getToken.getType == StringValue)
          throw new Exception("Invalid operator applied to string value")
        new Content((leftValue.toDouble - rightValue.toDouble).toString, new Token(NumberValue, 0, 0, new LexicalRange(0, 0, 0, 0)))
      case Asterisk =>
        if (leftContent.getToken.getType == StringValue || rightContent.getToken.getType == StringValue)
          throw new Exception("Invalid operator applied to string value")
        new Content((leftValue.toDouble * rightValue.toDouble).toString, new Token(NumberValue, 0, 0, new LexicalRange(0, 0, 0, 0)))
      case FrontSlash =>
        if (leftContent.getToken.getType == StringValue || rightContent.getToken.getType == StringValue)
          throw new Exception("Invalid operator applied to string value")
        new Content((leftValue.toDouble / rightValue.toDouble).toString, new Token(NumberValue, 0, 0, new LexicalRange(0, 0, 0, 0)))
    }
  }

  /** Indicates if operator 1 has precedence over operator 2
   *
   * @return true if operator 1 has precedence over operator 2
   */
  def hasPrecedence(operator1: TokenType, operator2: TokenType): Boolean = {
    if (operator2 == OpenParenthesis || operator2 == ClosedParenthesis)
      false
    else if ((operator1 == Asterisk || operator1 == FrontSlash) && (operator2 == Plus || operator2 == Minus))
      false
    else
      true
  }

  def calculate(root: AbstractSyntaxTree): Content[String] = {
    root.nodes.foreach(node =>
      node.content.getToken.getType match {
        case NumberValue | StringValue => values.push(node.content)
        case Identifier =>
          val name = node.content.getContent
          val value = variables(name)._1.get
          val dataType = variables(name)._2
          val tokenType = if (dataType == "String") StringValue else NumberValue
          values.push(new Content(value, new Token(tokenType, 0, 0, new LexicalRange(0, 0, 0, 0))))
        case OpenParenthesis => operators.push(OpenParenthesis)
        case ClosedParenthesis =>
          while (operators.head != OpenParenthesis) {
            val rightContent = values.pop
            val leftContent = values.pop
            values.push(applyOperator(operators.pop, leftContent, rightContent))
          }
          operators.pop
        case Plus | Minus | Asterisk | FrontSlash =>
          while (operators.nonEmpty && hasPrecedence(node.content.getToken.getType, operators.head)) {
            val rightContent = values.pop
            val leftContent = values.pop
            values.push(applyOperator(operators.pop, leftContent, rightContent))
          }
          operators.push(node.content.getToken.getType)
        case Empty =>
          if (node.content.getContent == "Expression") {
            values.push(calculate(node))
          }
      }
    )

    while (operators.nonEmpty) {
      val rightContent = values.pop
      val leftContent = values.pop
      values.push(applyOperator(operators.pop, leftContent, rightContent))
    }

    values.pop
  }
}
