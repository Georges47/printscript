package interpreter

import abstractSyntaxTree.AbstractSyntaxTree
import token.types.Identifier

class Interpreter {
  private var variables = Map[String, (Option[String], String)]()

  def interpret(abstractSyntaxTree: AbstractSyntaxTree): Unit = {
    abstractSyntaxTree.nodes.foreach(node => node.content.getContent match {
      case "Declaration" =>
        variables += interpretDeclaration(node)
      case "Assignment" =>
        variables += interpretAssignation(node)
      case "DeclarationAndAssignment" =>
        variables += interpretAssignationAndDeclaration(node)
      case "Println" =>
        println(
          if (node.nodes.head.content.getContent == "Expression") {
            ExpressionCalculator(variables).calculate(node.nodes.head).getContent
          } else if (node.nodes.head.content.getToken.getType == Identifier) {
            variables(node.nodes.head.content.getContent)._1.get
          } else {
            node.nodes.head.content.getContent //nodes.toString
          }
        )
      case "Expression" =>
        ExpressionCalculator(variables).calculate(node)
      case _ =>
    })

    println()
    variables foreach {case (key, value) => println(s"$key: $value")}
  }

  private def interpretDeclaration(node: AbstractSyntaxTree): (String, (Option[String], String)) = {
    val variableName = node.nodes.head.content.getContent
    val variableDataType = node.nodes(1).content.getContent
    variableName -> (None, variableDataType)
  }

  private def interpretAssignationAndDeclaration(node: AbstractSyntaxTree): (String, (Option[String], String)) = {
    val variableName = node.nodes.head.content.getContent
    val variableDataType = node.nodes(1).content.getContent
//    val variableValue = node.nodes(2).content.getContent
    val variableContent = ExpressionCalculator(variables).calculate(node.nodes(2))
    variableName -> (Some(variableContent.getContent), variableDataType)
  }

  private def interpretAssignation(node: AbstractSyntaxTree): (String, (Option[String], String)) = {
    val variableName = node.nodes.head.content.getContent
    val variableDataType = variables(variableName)._2
    val variableContent = ExpressionCalculator(variables).calculate(node.nodes(1)) //node.nodes(1).content.getContent
    variableName -> (Some(variableContent.getContent), variableDataType)
  }

}
