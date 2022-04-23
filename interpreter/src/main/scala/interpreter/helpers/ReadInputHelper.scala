package interpreter.helpers

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.IdentifierTable

case class ReadInputHelper() extends InterpreterHelper {
  override def interpret(
      ast: AbstractSyntaxTree,
      constants: IdentifierTable,
      variables: IdentifierTable
  ): Unit = {
    scala.io.StdIn.readLine(
      ast.nodes.head.root.value.stripPrefix("\"").stripSuffix("\"")
    )
  }
}
