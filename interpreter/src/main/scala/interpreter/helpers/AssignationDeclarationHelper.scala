package interpreter.helpers

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.{IdentifierTable, calculators}
import interpreter.calculators.ExpressionCalculator
import token.types.{BooleanDataType, ConstantIdentifier, ReadInput}

case class AssignationDeclarationHelper() extends InterpreterHelper {
  override def interpret(
      ast: AbstractSyntaxTree,
      constants: IdentifierTable,
      variables: IdentifierTable
  ): Unit = {
    val identifierName = ast.nodes.head.root.value
    val identifierDataType = ast.nodes(1).root

    if (identifierDataType.tokenType == BooleanDataType) {
      val identifierValue =
        ExpressionCalculator(variables, constants).calculate(ast.nodes(2)).value
      if (ast.nodes.head.root.tokenType == ConstantIdentifier) {
        constants.add(identifierName, identifierValue, identifierDataType.value)
      } else {
        variables.add(identifierName, identifierValue, identifierDataType.value)
      }
    } else {
      if (ast.nodes(2).root.tokenType == ReadInput) {
        val message = ast.nodes(3).root.value
        val input =
          scala.io.StdIn.readLine(message.stripPrefix("\"").stripSuffix("\""))
        if (ast.nodes.head.root.tokenType == ConstantIdentifier) {
          constants.add(identifierName, input, identifierDataType.value)
        } else {
          variables.add(identifierName, input, identifierDataType.value)
        }
      } else {
        val identifierValue =
          calculators.ExpressionCalculator(variables, constants).calculate(ast.nodes(2))
        if (ast.nodes.head.root.tokenType == ConstantIdentifier) {
          constants.add(
            identifierName,
            identifierValue.value,
            identifierDataType.value
          )
        } else {
          variables.add(
            identifierName,
            identifierValue.value,
            identifierDataType.value
          )
        }
      }
    }
  }
}
