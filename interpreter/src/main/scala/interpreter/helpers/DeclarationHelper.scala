package interpreter.helpers

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.IdentifierTable

case class DeclarationHelper() extends InterpreterHelper {
  override def interpret(node: AbstractSyntaxTree, constants: IdentifierTable, variables: IdentifierTable): Unit = {
    val variableName = node.nodes.head.root.value
    val variableDataType = node.nodes(1).root.value
    variables.add(variableName, variableDataType)
  }
}
