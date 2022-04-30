package interpreter.helpers

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.IdentifierTable
import interpreter.calculators.ExpressionCalculator

case class ExpressionHelper() extends InterpreterHelper {
  override def interpret(
      ast: AbstractSyntaxTree,
      constants: IdentifierTable,
      variables: IdentifierTable
  ): Unit = {
    ExpressionCalculator(variables, constants).calculate(ast)
  }
}
