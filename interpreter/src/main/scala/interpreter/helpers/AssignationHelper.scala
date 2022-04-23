package interpreter.helpers

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.{ExpressionCalculator, IdentifierTable}

case class AssignationHelper() extends InterpreterHelper {
  override def interpret(ast: AbstractSyntaxTree, constants: IdentifierTable, variables: IdentifierTable): Unit = {
    val name = ast.nodes.head.root.value
    val lexicalRange = ast.nodes.head.root.lexicalRange.get
    if (variables.check(name)) {
      val dataType = variables.dataType(name)
      val result = ExpressionCalculator(variables, constants).calculate(ast.nodes(1))
      variables.add(name, result.value, dataType)
    } else if (constants.check(name)) {
      throw new Exception(s"Reassigning constant at line ${lexicalRange.getStartLine}, column ${lexicalRange.getStartCol}")
    } else {
      throw new Exception(s"Non existing identifier at line ${lexicalRange.getStartLine}, column ${lexicalRange.getStartCol}")
    }
  }
}
