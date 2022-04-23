package interpreter.helpers

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.IdentifierTable

object InterpreterHelper {
  val helpers = Map(
    "Declaration" -> DeclarationHelper(),
    "Assignment" -> AssignationHelper(),
    "DeclarationAndAssignment" -> AssignationDeclarationHelper(),
    "If" -> IfHelper(),
    "Println" -> PrintlnHelper(),
    "ReadInput" -> ReadInputHelper(),
    "Expression" -> ExpressionHelper()
  )
}

trait InterpreterHelper {
  def interpret(node: AbstractSyntaxTree, constants: IdentifierTable, variables: IdentifierTable): Unit
}
