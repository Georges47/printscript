package interpreter.helpers

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.{IdentifierTable, Interpreter}
import token.types.{BooleanValue, Identifier}

case class IfHelper(interpreter: Interpreter) extends InterpreterHelper {
  override def interpret(
      ast: AbstractSyntaxTree,
      constants: IdentifierTable,
      variables: IdentifierTable
  ): Unit = {
    val conditionTokenType = ast.nodes.head.root.tokenType
    var conditionString: String = ""
    conditionTokenType match {
      case BooleanValue =>
        conditionString = ast.nodes.head.root.value
      case Identifier =>
        val conditionIdentifierName = ast.nodes.head.root.value
        if (variables.check(conditionIdentifierName)) {
          conditionString = variables.value(conditionIdentifierName).get
        } else {
          conditionString = constants.value(conditionIdentifierName).get
        }
    }
    if (conditionString.toBooleanOption.get) {
      val ifBlock = ast.nodes(1)
      interpreter.interpret(ifBlock)
    } else if (ast.nodes.size == 3) {
      val elseBlock = ast.nodes(2)
      interpreter.interpret(elseBlock)
    }
  }
}
