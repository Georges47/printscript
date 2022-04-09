package interpreter

import abstractSyntaxTree.AbstractSyntaxTree
import token.types.{Assignment, Declaration, DeclarationAndAssignment, Expression, Identifier, Println}

class Interpreter {
  private var variables = Map[String, (Option[String], String)]()

  def interpret(abstractSyntaxTree: AbstractSyntaxTree): Unit = {
    abstractSyntaxTree.nodes.foreach(node => node.root.tokenType match {
      case Declaration =>
        variables += interpretDeclaration(node)
      case Assignment =>
        variables += interpretAssignation(node)
      case DeclarationAndAssignment =>
        variables += interpretAssignationAndDeclaration(node)
      case Println =>
        println(
          if (node.nodes.head.root.tokenType == Expression) {
            ExpressionCalculator(variables).calculate(node.nodes.head).value
          } else if (node.nodes.head.root.tokenType == Identifier) {
            variables(node.nodes.head.root.value)._1.get
          } else {
            node.nodes.head.root.value //nodes.toString
          }
        )
      case Expression =>
        ExpressionCalculator(variables).calculate(node)
      case _ =>
    })
    println()
    variables foreach {case (key, value) => println(s"$key: $value")}
  }

  def expectVariableToExistWithValueAndDataType(name: String, dataType: String, value: String): Boolean = {
    println(variables(name))
    variables.contains(name) && variables(name) == (Some(value), dataType)
  }

  private def interpretDeclaration(node: AbstractSyntaxTree): (String, (Option[String], String)) = {
    val variableName = node.nodes.head.root.value
    val variableDataType = node.nodes(1).root.value
    variableName -> (None, variableDataType)
  }

  private def interpretAssignationAndDeclaration(node: AbstractSyntaxTree): (String, (Option[String], String)) = {
    val variableName = node.nodes.head.root.value
    val variableDataType = node.nodes(1).root.value
    val variableContent = ExpressionCalculator(variables).calculate(node.nodes(2))
    variableName -> (Some(variableContent.value), variableDataType)
  }

  private def interpretAssignation(node: AbstractSyntaxTree): (String, (Option[String], String)) = {
    val variableName = node.nodes.head.root.value
    val variableDataType = variables(variableName)._2
    val variableContent = ExpressionCalculator(variables).calculate(node.nodes(1))
    variableName -> (Some(variableContent.value), variableDataType)
  }

}
