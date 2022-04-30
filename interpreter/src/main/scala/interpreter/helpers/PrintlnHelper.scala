package interpreter.helpers

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.IdentifierTable
import interpreter.calculators.ExpressionCalculator

case class PrintlnHelper() extends InterpreterHelper {
  override def interpret(
      ast: AbstractSyntaxTree,
      constants: IdentifierTable,
      variables: IdentifierTable
  ): Unit = {
    val value =
      ExpressionCalculator(variables, constants).calculate(ast.nodes.head).value
    println(value)
  }
}
