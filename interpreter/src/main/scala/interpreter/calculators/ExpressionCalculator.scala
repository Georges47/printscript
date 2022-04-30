package interpreter.calculators

import abstractSyntaxTree.{AbstractSyntaxTree, Node}
import interpreter.IdentifierTable
import org.austral.ingsis.printscript.common.TokenType
import token.types._

import scala.collection.mutable

/** Calculates the value of an expression using the Shunting Yard algorithm
  * https://en.wikipedia.org/wiki/Shunting_yard_algorithm
  *
  * @param variables contains variables declared and assigned in the program
  */
case class ExpressionCalculator(
    variables: IdentifierTable,
    constants: IdentifierTable
) {
  val values: mutable.Stack[Node] = mutable.Stack().empty
  val operators: mutable.Stack[TokenType] = mutable.Stack().empty
  val helpers: Map[String, CalculatorHelper] = Map(
    "add" -> AddHelper(),
    "subtract" -> SubtractHelper(),
    "multiply" -> MultiplyHelper(),
    "divide" -> DivideHelper()
  )

  def calculate(root: AbstractSyntaxTree): Node = {
    root.nodes.foreach(node => {
      node.root.tokenType match {
        case NumberValue | StringValue | BooleanValue => values.push(node.root)
        case Identifier =>
          if (variables.check(node.root.value)) {
            val name = node.root.value
            val value = variables.value(name).get
            val dataType = variables.dataType(name)
            val tokenType = dataType match {
              case "string"  => StringValue
              case "number"  => NumberValue
              case "boolean" => BooleanValue
            }
            values.push(Node(value, tokenType))
          } else {
            val name = node.root.value
            val value = constants.value(name).get
            val dataType = constants.dataType(name)
            val tokenType = dataType match {
              case "string"  => StringValue
              case "number"  => NumberValue
              case "boolean" => BooleanValue
            }
            values.push(Node(value, tokenType))
          }
        case OpenParenthesis => operators.push(OpenParenthesis)
        case ClosedParenthesis =>
          while (operators.head != OpenParenthesis) {
            val rightContent = values.pop
            val leftContent = values.pop
            values.push(applyOperator(operators.pop, leftContent, rightContent))
          }
          operators.pop
        case Plus | Minus | Asterisk | FrontSlash | And | Or =>
          while (
            operators.nonEmpty &&
            (operators.head == Plus || operators.head == Minus || operators.head == Asterisk || operators.head == FrontSlash) &&
            hasPrecedence(
              operators.head,
              node.root.tokenType
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
        val result = leftValue.toBooleanOption.get && rightValue.toBooleanOption.get
        Node(result.toString, BooleanValue)
      case Or =>
        val result = leftValue.toBooleanOption.get || rightValue.toBooleanOption.get
        Node(result.toString, BooleanValue)
      case Plus       => handleMathOperand(leftNode, rightNode, "add")
      case Minus      => handleMathOperand(leftNode, rightNode, "subtract")
      case Asterisk   => handleMathOperand(leftNode, rightNode, "multiply")
      case FrontSlash => handleMathOperand(leftNode, rightNode, "divide")
    }
  }

  /** Indicates if operator 1 has precedence over operator 2
    * @return true if operator 1 has precedence over operator 2
    */
  private def hasPrecedence(operator1: TokenType, operator2: TokenType): Boolean = {
    if (operator2 == OpenParenthesis || operator2 == ClosedParenthesis)
      false
    else if (
      (operator2 == Asterisk || operator2 == FrontSlash) && (operator1 == Plus || operator1 == Minus)
    )
      false
    else if (operator1 == Or && operator2 == And)
      false
    else
      true
  }

  private def handleMathOperand(leftNode: Node, rightNode: Node, operation: String): Node = {
    val leftValue = leftNode.value
    val rightValue = rightNode.value
    if (operandIsString(leftNode, rightNode)) {
      Node(helpers(operation).calculateStrings(leftValue, rightValue), StringValue)
    } else {
      if (operandsAreInts(leftValue, rightValue)) {
        Node(helpers(operation).calculateInts(leftValue, rightValue), NumberValue)
      } else {
        Node(helpers(operation).calculateDoubles(leftValue, rightValue), NumberValue)
      }
    }
  }

  private def operandIsString(operand1: Node, operand2: Node): Boolean = {
    operand1.tokenType == StringValue || operand2.tokenType == StringValue
  }

  private def operandsAreInts(operand1: String, operand2: String): Boolean = {
    operand1.toIntOption.isDefined && operand2.toIntOption.isDefined
  }

}
