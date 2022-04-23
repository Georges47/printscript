package interpreter

import abstractSyntaxTree.{AbstractSyntaxTree, Node}
import org.austral.ingsis.printscript.common.TokenType
import token.types._

import scala.collection.mutable

/** Calculates the value of an expression using the Shunting yard algorithm
  * https://en.wikipedia.org/wiki/Shunting_yard_algorithm
  * @param variables contains variables declared and assigned in the program
  */
case class ExpressionCalculator(
    variables: IdentifierTable,
    constants: IdentifierTable
) {
  val values: mutable.Stack[Node] = mutable.Stack().empty
  val operators: mutable.Stack[TokenType] = mutable.Stack().empty

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
//            val tokenType = if (dataType == "String") StringValue else NumberValue
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
      case Plus =>
        if (
          leftNode.tokenType == StringValue || rightNode.tokenType == StringValue
        ) {
          Node(
            leftValue.replaceAll("^\"|\"$", "") + rightValue
              .replaceAll("^\"|\"$", ""),
            StringValue
          )
        } else {
          if (
            leftValue.toIntOption.isDefined && rightValue.toIntOption.isDefined
          ) {
            Node((leftValue.toInt + rightValue.toInt).toString, NumberValue)
          } else {
            Node(
              (leftValue.toDouble + rightValue.toDouble).toString,
              NumberValue
            )
          }
        }
      case Minus =>
        if (
          leftNode.tokenType == StringValue || rightNode.tokenType == StringValue
        ) {
          throw new Exception("Invalid operator applied to string value")
        }
        if (
          leftValue.toIntOption.isDefined && rightValue.toIntOption.isDefined
        ) {
          Node((leftValue.toInt - rightValue.toInt).toString, NumberValue)
        } else {
          Node((leftValue.toDouble - rightValue.toDouble).toString, NumberValue)
        }
      case Asterisk =>
        if (
          leftNode.tokenType == StringValue || rightNode.tokenType == StringValue
        )
          throw new Exception("Invalid operator applied to string value")
        if (
          leftValue.toIntOption.isDefined && rightValue.toIntOption.isDefined
        ) {
          Node((leftValue.toInt * rightValue.toInt).toString, NumberValue)
        } else {
          Node((leftValue.toDouble * rightValue.toDouble).toString, NumberValue)
        }
      case FrontSlash =>
        if (
          leftNode.tokenType == StringValue || rightNode.tokenType == StringValue
        )
          throw new Exception("Invalid operator applied to string value")
//        if(leftValue.toIntOption.isDefined && rightValue.toIntOption.isDefined) {
//          Node((leftValue.toInt / rightValue.toInt).toString, NumberValue)
//        } else {
        Node((leftValue.toDouble / rightValue.toDouble).toString, NumberValue)
//        }
    }
  }

  /** Indicates if operator 1 has precedence over operator 2
    * @return true if operator 1 has precedence over operator 2
    */
  def hasPrecedence(operator1: TokenType, operator2: TokenType): Boolean = {
    if (operator2 == OpenParenthesis || operator2 == ClosedParenthesis)
      false
    else if (
      (operator1 == Asterisk || operator1 == FrontSlash) && (operator2 == Plus || operator2 == Minus)
    )
      false
    else if (operator1 == Or && operator2 == And)
      false
    else
      true
  }
}
