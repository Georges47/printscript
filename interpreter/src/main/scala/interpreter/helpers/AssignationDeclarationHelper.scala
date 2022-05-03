package interpreter.helpers

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.{IdentifierTable, calculators}
import interpreter.calculators.ExpressionCalculator
import interpreter.inputs.InputProvider
import token.types.{BooleanDataType, ConstantIdentifier, ReadInput}

case class AssignationDeclarationHelper(inputProvider: InputProvider) extends InterpreterHelper {
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

      if (!correctDataType(identifierValue, identifierDataType.value)) {
        throw new Exception(
          s"Wrong datatype in assignation and declaration of identifier $identifierName"
        )
      }

      if (ast.nodes.head.root.tokenType == ConstantIdentifier) {
        constants.add(identifierName, identifierValue, identifierDataType.value)
      } else {
        variables.add(identifierName, identifierValue, identifierDataType.value)
      }
    } else {
      if (ast.nodes(2).root.tokenType == ReadInput) {
        val message = ast.nodes(3).root.value
        print(message.stripPrefix("\"").stripSuffix("\""))
        val input = inputProvider.input(identifierName)
        if (ast.nodes.head.root.tokenType == ConstantIdentifier) {
          constants.add(identifierName, input, identifierDataType.value)
        } else {
          variables.add(identifierName, input, identifierDataType.value)
        }
      } else {
        val identifierValue =
          calculators.ExpressionCalculator(variables, constants).calculate(ast.nodes(2))

        if (!correctDataType(identifierValue.value, identifierDataType.value)) {
          throw new Exception(
            s"Wrong datatype in assignation and declaration of identifier $identifierName"
          )
        }

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

  private def correctDataType(value: String, dataType: String): Boolean = {
    (isNumeric(value) && dataType == "number") ||
    (isBoolean(value) && dataType == "boolean") ||
    (dataType == "string")
  }

  private def isNumeric(value: String): Boolean = {
    scala.util.Try(value.toDouble).isSuccess
  }

  private def isBoolean(value: String): Boolean = {
    value == "true" || value == "false"
  }
}
