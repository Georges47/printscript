package interpreter.helpers

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.{ExpressionCalculator, IdentifierTable}
import token.types.{BooleanDataType, ConstantIdentifier, ReadInput}

case class AssignationDeclarationHelper() extends InterpreterHelper {
  override def interpret(node: AbstractSyntaxTree, constants: IdentifierTable, variables: IdentifierTable): Unit = {
    val identifierName = node.nodes.head.root.value
    val identifierDataType = node.nodes(1).root
    if (identifierDataType.tokenType == BooleanDataType) {
      val identifierValue = ExpressionCalculator(variables, constants).calculate(node.nodes(2)).value
      if (node.nodes.head.root.tokenType == ConstantIdentifier) {
        constants.add(identifierName, identifierValue, identifierDataType.value)
      } else {
        variables.add(identifierName, identifierValue, identifierDataType.value)
      }
    } else {
      if (node.nodes(2).root.tokenType == ReadInput) {
        val message = node.nodes(3).root.value
        val input = scala.io.StdIn.readLine(message.stripPrefix("\"").stripSuffix("\""))
        if (node.nodes.head.root.tokenType == ConstantIdentifier) {
          constants.add(identifierName, input, identifierDataType.value)
        } else {
          variables.add(identifierName, input, identifierDataType.value)
        }
      } else {
        val identifierValue = ExpressionCalculator(variables, constants).calculate(node.nodes(2))
        if (node.nodes.head.root.tokenType == ConstantIdentifier) {
          constants.add(identifierName, identifierValue.value, identifierDataType.value)
        } else {
          variables.add(identifierName, identifierValue.value, identifierDataType.value)
        }
      }
    }
  }
}
