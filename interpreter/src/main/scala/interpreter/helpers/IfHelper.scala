package interpreter.helpers

import abstractSyntaxTree.AbstractSyntaxTree
import interpreter.{IdentifierTable, Interpreter}
import token.types.{BooleanValue, Identifier}

case class IfHelper() extends InterpreterHelper {
  override def interpret(node: AbstractSyntaxTree, constants: IdentifierTable, variables: IdentifierTable): Unit = {
    val conditionTokenType = node.nodes.head.root.tokenType
    var conditionString: String = ""
    conditionTokenType match {
      case BooleanValue =>
        conditionString = node.nodes.head.root.value
      case Identifier =>
        val conditionIdentifierName = node.nodes.head.root.value
        if (variables.check(conditionIdentifierName)) {
          conditionString = variables.value(conditionIdentifierName).get
        } else {
          conditionString = constants.value(conditionIdentifierName).get
        }
    }
    if (conditionString.toBooleanOption.get) {
      val ifBlock = node.nodes(1)
      (new Interpreter).interpret(ifBlock)
    } else if (node.nodes.size == 3) {
      val elseBlock = node.nodes(2)
      (new Interpreter).interpret(elseBlock)
    }
  }
}
