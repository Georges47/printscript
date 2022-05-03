package interpreter.helpers

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.IdentifierTable

case class ReadInputHelper() extends InterpreterHelper {
  override def interpret(
      ast: AbstractSyntaxTree,
      constants: IdentifierTable,
      variables: IdentifierTable
  ): Unit = {
    var message = ast.nodes.head.root.value
    if (message.head == '\"') message = message.drop(1)
    if (message.last == '\"') message = message.dropRight(1)
    print(message)
    scala.io.StdIn.readLine()
  }
}
