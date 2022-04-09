package interpreter

import abstractSyntaxTree.{AbstractSyntaxTree, Node}
import org.austral.ingsis.printscript.common.TokenType
import token.types._

import scala.collection.mutable

case class ExpressionCalculator(variables: Map[String, (Option[String], String)]) {
  val values: mutable.Stack[Node] = mutable.Stack().empty
  val operators: mutable.Stack[TokenType] = mutable.Stack().empty

  /** Applies the operator to the two corresponding values
   *
   * @param operator operator to be applied
   * @param leftNode first operand
   * @param rightNode second operand
   * @return the result of the calculation
   */
  def applyOperator(operator: TokenType, leftNode: Node, rightNode: Node): Node = {
    val leftValue = leftNode.value
    val rightValue = rightNode.value
    operator match {
      case Plus =>
        if (leftNode.tokenType == StringValue || rightNode.tokenType == StringValue) {
          Node(leftValue.replaceAll("^\"|\"$", "") + rightValue.replaceAll("^\"|\"$", ""), StringValue)
        } else {
          Node((leftValue.toDouble + rightValue.toDouble).toString, NumberValue)
        }
      case Minus =>
        if (leftNode.tokenType == StringValue || rightNode.tokenType == StringValue)
          throw new Exception("Invalid operator applied to string value")
        Node((leftValue.toDouble - rightValue.toDouble).toString, NumberValue)
      case Asterisk =>
        if (leftNode.tokenType == StringValue || rightNode.tokenType == StringValue)
          throw new Exception("Invalid operator applied to string value")
        Node((leftValue.toDouble * rightValue.toDouble).toString, NumberValue)
      case FrontSlash =>
        if (leftNode.tokenType == StringValue || rightNode.tokenType == StringValue)
          throw new Exception("Invalid operator applied to string value")
        Node((leftValue.toDouble / rightValue.toDouble).toString, NumberValue)
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

  def calculate(root: AbstractSyntaxTree): Node = {
    root.nodes.foreach(node =>
      node.root.tokenType match {
        case NumberValue | StringValue => values.push(node.root)
        case Identifier =>
          val name = node.root.value
          val value = variables(name)._1.get
          val dataType = variables(name)._2
          val tokenType = if (dataType == "String") StringValue else NumberValue
          values.push(Node(value, tokenType))
        case OpenParenthesis => operators.push(OpenParenthesis)
        case ClosedParenthesis =>
          while (operators.head != OpenParenthesis) {
            val rightContent = values.pop
            val leftContent = values.pop
            values.push(applyOperator(operators.pop, leftContent, rightContent))
          }
          operators.pop
        case Plus | Minus | Asterisk | FrontSlash =>
          while (operators.nonEmpty && hasPrecedence(node.root.tokenType, operators.head)) {
            val rightContent = values.pop
            val leftContent = values.pop
            values.push(applyOperator(operators.pop, leftContent, rightContent))
          }
          operators.push(node.root.tokenType)
        case Empty =>
          if (node.root.tokenType == Expression) {
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
