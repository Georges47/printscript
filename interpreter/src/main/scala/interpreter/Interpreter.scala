package interpreter

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.helpers.InterpreterHelper

class Interpreter {
  private val variables = IdentifierTable()
  private val constants = IdentifierTable()
  private val helpers = InterpreterHelper.helpers

  def interpret(abstractSyntaxTree: AbstractSyntaxTree): Unit = {
    abstractSyntaxTree.nodes.foreach(node => {
      val helperType = node.root.tokenType.toString
      if (helpers.contains(helperType)) {
        helpers(helperType).interpret(node, constants, variables)
      }
    })
  }

  def expectVariableToExistWithValueAndDataType(name: String, dataType: String, value: String): Boolean = {
    variables.check(name, value, dataType)
  }

}
