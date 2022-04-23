package interpreter

import abstractSyntaxTree.{AbstractSyntaxTree, Node}
import org.austral.ingsis.printscript.common.TokenType
import token.types._

import scala.collection.mutable

/** Calculates the value of an expression
  * @param variables contains variables declared and assigned in the program
  */
case class BooleanExpressionCalculator(
    variables: IdentifierTable,
    constants: IdentifierTable
) {
  val values: mutable.Stack[Node] = mutable.Stack().empty
  val operators: mutable.Stack[TokenType] = mutable.Stack().empty

  def calculate(root: AbstractSyntaxTree): Node = {
    root.nodes.foreach(node => {
      node.root.tokenType match {
        case BooleanValue => values.push(node.root)
        case Identifier =>
          if (variables.check(node.root.value)) {
            val name = node.root.value
            val value = variables.value(name).get
            values.push(Node(value, BooleanValue))
          } else {
            val name = node.root.value
            val value = constants.value(name).get
            values.push(Node(value, BooleanValue))
          }
        case OpenParenthesis => operators.push(OpenParenthesis)
        case ClosedParenthesis =>
          while (operators.head != OpenParenthesis) {
            val rightContent = values.pop
            val leftContent = values.pop
            values.push(applyOperator(operators.pop, leftContent, rightContent))
          }
          operators.pop
        case And | Or =>
          while (
            operators.nonEmpty && hasPrecedence(
              node.root.tokenType,
              operators.head
            )
          ) {
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
    })

    while (operators.nonEmpty) {
      val rightContent = values.pop
      val leftContent = values.pop
      values.push(applyOperator(operators.pop, leftContent, rightContent))
    }

    values.pop
  }

  /** Applies the operator to the two corresponding values
    * @param operator operator to be applied
    * @param leftNode first operand
    * @param rightNode second operand
    * @return the result of the calculation
    */
  def applyOperator(
      operator: TokenType,
      leftNode: Node,
      rightNode: Node
  ): Node = {
    val leftValue = leftNode.value
    val rightValue = rightNode.value
    operator match {
      case And =>
        val result =
          leftValue.toBooleanOption.get && rightValue.toBooleanOption.get
        Node(result.toString, BooleanValue)
      case Or =>
        val result =
          leftValue.toBooleanOption.get || rightValue.toBooleanOption.get
        Node(result.toString, BooleanValue)
    }
  }

  /** Indicates if operator 1 has precedence over operator 2
    * @return true if operator 1 has precedence over operator 2
    */
  def hasPrecedence(operator1: TokenType, operator2: TokenType): Boolean = {
    if (operator2 == OpenParenthesis || operator2 == ClosedParenthesis)
      false
    else if (operator1 == Or && operator2 == And)
      false
    else
      true
  }
}
