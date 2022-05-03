package interpreter.helpers

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.{IdentifierTable, Interpreter}

object InterpreterHelper {
  def helpers(interpreter: Interpreter): Map[String, InterpreterHelper] = {
    Map(
      "Declaration" -> DeclarationHelper(),
      "Assignment" -> AssignationHelper(),
      "DeclarationAndAssignment" -> AssignationDeclarationHelper(interpreter.inputProvider),
      "If" -> IfHelper(interpreter),
      "Println" -> PrintlnHelper(),
      "ReadInput" -> ReadInputHelper(),
      "Expression" -> ExpressionHelper()
    )
  }
}

trait InterpreterHelper {
  def interpret(
      ast: AbstractSyntaxTree,
      constants: IdentifierTable,
      variables: IdentifierTable
  ): Unit
}
