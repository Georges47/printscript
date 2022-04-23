package interpreter.helpers

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.{ExpressionCalculator, IdentifierTable}

case class PrintlnHelper() extends InterpreterHelper {
  override def interpret(node: AbstractSyntaxTree, constants: IdentifierTable, variables: IdentifierTable): Unit = {
    val value = ExpressionCalculator(variables, constants).calculate(node.nodes.head).value
    println(value)
  }
}
