package interpreter

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.calculators.ExpressionCalculator
import interpreter.helpers._
import interpreter.inputs.{ConsoleInputProvider, InterpreterInputProvider}

class Interpreter(
    constants: IdentifierTable = IdentifierTable(),
    variables: IdentifierTable = IdentifierTable(),
    testMode: Boolean = false
) {
  var inputProvider: InterpreterInputProvider = new ConsoleInputProvider()
  var logs: java.util.ArrayList[String] = new java.util.ArrayList()
  private val helpers = InterpreterHelper.helpers(this)

  def interpret(abstractSyntaxTree: AbstractSyntaxTree): Unit = {
    abstractSyntaxTree.nodes.foreach(node => {
      val helperType = node.root.tokenType.toString
      if (helpers.contains(helperType)) {
        if (testMode && helperType == "Println") {
          var log = ExpressionCalculator(variables, constants).calculate(node.nodes.head).value
          if (log.head == '\"') log = log.drop(1)
          if (log.last == '\"') log = log.dropRight(1)
          logs.add(log)
        } else if (testMode && helperType == "ReadInput") {
          var log = ExpressionCalculator(variables, constants).calculate(node.nodes.head).value
          if (log.head == '\"') log = log.drop(1)
          if (log.last == '\"') log = log.dropRight(1)
          logs.add(log)
        } else {
          helpers(helperType).interpret(node, constants, variables)
        }
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
