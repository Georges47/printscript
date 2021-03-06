package interpreter

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.helpers.InterpreterHelper

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
