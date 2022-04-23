package interpreter.helpers

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.IdentifierTable

case class ReadInputHelper() extends InterpreterHelper {
  override def interpret(node: AbstractSyntaxTree, constants: IdentifierTable, variables: IdentifierTable): Unit = {
    scala.io.StdIn.readLine(node.nodes.head.root.value.stripPrefix("\"").stripSuffix("\""))
  }
}
