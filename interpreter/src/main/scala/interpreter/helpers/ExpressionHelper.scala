package interpreter.helpers

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.{ExpressionCalculator, IdentifierTable}

case class ExpressionHelper() extends InterpreterHelper {
  override def interpret(ast: AbstractSyntaxTree, constants: IdentifierTable, variables: IdentifierTable): Unit = {
    ExpressionCalculator(variables, constants).calculate(ast)
  }
}
