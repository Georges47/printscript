package interpreter

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.calculators.ExpressionCalculator
import interpreter.helpers._

class Interpreter(
    constants: IdentifierTable = IdentifierTable(),
    variables: IdentifierTable = IdentifierTable()
) {
  private val helpers = InterpreterHelper.helpers

  def interpret(abstractSyntaxTree: AbstractSyntaxTree): Unit = {
    abstractSyntaxTree.nodes.foreach(node => {
      val helperType = node.root.tokenType.toString
      if (helpers.contains(helperType)) {
        helpers(helperType).interpret(node, constants, variables)
      }
    })
  }

  def testInterpret(abstractSyntaxTree: AbstractSyntaxTree): String = {
    var logs = ""
    abstractSyntaxTree.nodes.foreach(node => {
      val helperType = node.root.tokenType.toString
      if (helpers.contains(helperType)) {
        if (helperType == "Println") {
          logs += ExpressionCalculator(variables, constants).calculate(node.nodes.head).value
        } else {
          helpers(helperType).interpret(node, constants, variables)
        }
      }
    })
    logs
  }

  def expectVariableToExistWithValueAndDataType(
      name: String,
      dataType: String,
      value: String
  ): Boolean = {
    variables.check(name, value, dataType)
  }

  def expectConstantToExistWithValueAndDataType(
      name: String,
      dataType: String,
      value: String
  ): Boolean = {
    constants.check(name, value, dataType)
  }

}
