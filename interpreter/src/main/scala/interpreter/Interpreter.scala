package interpreter

import abstractSyntaxTree.AbstractSyntaxTree
import token.types.{Assignment, Declaration, DeclarationAndAssignment, Expression, Identifier, Println}

class Interpreter {
  private val variables = VariableTable()

  def interpret(abstractSyntaxTree: AbstractSyntaxTree): VariableTable = {
    abstractSyntaxTree.nodes.foreach(node => node.root.tokenType match {
      case Declaration => interpretDeclaration(node, variables)
      case Assignment => interpretAssignation(node)
      case DeclarationAndAssignment => interpretAssignationAndDeclaration(node)
      case Println =>
        println(
          if (node.nodes.head.root.tokenType == Expression) {
            ExpressionCalculator(variables).calculate(node.nodes.head).value
          } else if (node.nodes.head.root.tokenType == Identifier) {
            variables.value(node.nodes.head.root.value).get
          } else {
            node.nodes.head.root.value
          }
        )
      case Expression =>
        ExpressionCalculator(variables).calculate(node)
      case _ =>
    })
    variables
  }

  def expectVariableToExistWithValueAndDataType(name: String, dataType: String, value: String): Boolean = {
    variables.check(name, value, dataType)
  }

  private def interpretDeclaration(node: AbstractSyntaxTree, variables: VariableTable): Unit = {
    val variableName = node.nodes.head.root.value
    val variableDataType = node.nodes(1).root.value
    variables.add(variableName, variableDataType)
  }

  private def interpretAssignationAndDeclaration(node: AbstractSyntaxTree): Unit = {
    val name = node.nodes.head.root.value
    val dataType = node.nodes(1).root.value
    val result = ExpressionCalculator(variables).calculate(node.nodes(2))
    variables.add(name, result.value, dataType)
  }

  private def interpretAssignation(node: AbstractSyntaxTree): Unit = {
    val name = node.nodes.head.root.value
    val dataType = variables.dataType(name)
    val result = ExpressionCalculator(variables).calculate(node.nodes(1))
    variables.add(name, result.value, dataType)
  }

}
