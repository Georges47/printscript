package interpreter.helpers

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.IdentifierTable

case class DeclarationHelper() extends InterpreterHelper {
  override def interpret(
      ast: AbstractSyntaxTree,
      constants: IdentifierTable,
      variables: IdentifierTable
  ): Unit = {
    val variableName = ast.nodes.head.root.value
    val variableDataType = ast.nodes(1).root.value
    variables.add(variableName, variableDataType)
  }
}
