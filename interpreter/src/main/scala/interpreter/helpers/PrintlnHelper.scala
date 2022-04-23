package interpreter.helpers

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.{ExpressionCalculator, IdentifierTable}

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
