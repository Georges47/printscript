package interpreter

import abstractSyntaxTree.AbstractSyntaxTree
import token.types.{Assignment, BooleanDataType, BooleanValue, ConstantIdentifier, Declaration, DeclarationAndAssignment, Expression, Identifier, If, Println, ReadInput, VariableIdentifier}

import scala.io.StdIn
import scala.io.StdIn.readLine

class Interpreter {
  private val variables = IdentifierTable()
  private val constants = IdentifierTable()

  def interpret(abstractSyntaxTree: AbstractSyntaxTree): IdentifierTable = {
    abstractSyntaxTree.nodes.foreach(node => node.root.tokenType match {
      case Declaration => interpretDeclaration(node, variables)
      case Assignment => interpretAssignation(node)
      case DeclarationAndAssignment => interpretAssignationAndDeclaration(node)
      case If => interpretIf(node)
      case Println =>
        println(
          if (node.nodes.head.root.tokenType == Expression) {
            ExpressionCalculator(variables, constants).calculate(node.nodes.head).value
//          } else if (node.nodes.head.root.tokenType == VariableIdentifier) {
//            println("2")
//            variables.value(node.nodes.head.root.value).get
//          } else {
//            println("3")
//            node.nodes.head.root.value
          }
        )
      case ReadInput =>
        val input = scala.io.StdIn.readLine(node.nodes.head.root.value.stripPrefix("\"").stripSuffix("\""))
      case Expression =>
        ExpressionCalculator(variables, constants).calculate(node)
      case _ =>
    })
    println("Variables:")
    println(variables.toString)
    println("Constants:")
    println(constants.toString)
    variables
  }

  def expectVariableToExistWithValueAndDataType(name: String, dataType: String, value: String): Boolean = {
    variables.check(name, value, dataType)
  }

  private def interpretDeclaration(node: AbstractSyntaxTree, variables: IdentifierTable): Unit = {
    val variableName = node.nodes.head.root.value
    val variableDataType = node.nodes(1).root.value
    variables.add(variableName, variableDataType)
  }

  private def interpretAssignationAndDeclaration(node: AbstractSyntaxTree): Unit = {
    val identifierName = node.nodes.head.root.value
    val identifierDataType = node.nodes(1).root
    if (identifierDataType.tokenType == BooleanDataType) {
      // Chequear que el value (node.nodes(2).root.value) sea valido (true o false)
//      println(node.nodes(2).root.value)
//      println(node.nodes.head)
//      println(node.nodes(1))
//      println(node.nodes(2))
      val identifierValue = ExpressionCalculator(variables, constants).calculate(node.nodes(2)).value
      if (node.nodes.head.root.tokenType == ConstantIdentifier) {
        constants.add(identifierName, identifierValue, identifierDataType.value)
      } else {
        variables.add(identifierName, identifierValue, identifierDataType.value)
      }
    } else {
      val identifierValue = ExpressionCalculator(variables, constants).calculate(node.nodes(2))
      if (node.nodes.head.root.tokenType == ConstantIdentifier) {
        constants.add(identifierName, identifierValue.value, identifierDataType.value)
      } else {
        variables.add(identifierName, identifierValue.value, identifierDataType.value)
      }
    }
  }

  private def interpretAssignation(node: AbstractSyntaxTree): Unit = {
    val name = node.nodes.head.root.value
    val lexicalRange = node.nodes.head.root.lexicalRange.get
    if (variables.check(name)) {
      val dataType = variables.dataType(name)
      val result = ExpressionCalculator(variables, constants).calculate(node.nodes(1))
      variables.add(name, result.value, dataType)
    } else if(constants.check(name)) {
      throw new Exception(s"Reassigning constant at line ${lexicalRange.getStartLine}, column ${lexicalRange.getStartCol}")
    } else {
      throw new Exception(s"Non existing identifier at line ${lexicalRange.getStartLine}, column ${lexicalRange.getStartCol}")
    }
  }

  def interpretIf(node: AbstractSyntaxTree): Unit = {
    val conditionTokenType = node.nodes.head.root.tokenType
    var conditionValue: String = ""
    conditionTokenType match {
      case BooleanValue =>
        conditionValue = node.nodes.head.root.value
      case Identifier =>
        val conditionIdentifierName = node.nodes.head.root.value
        if (variables.check(conditionIdentifierName)) {
          conditionValue = variables.value(conditionIdentifierName).get
        } else {
          conditionValue = constants.value(conditionIdentifierName).get
        }
    }

    if (conditionValue.toBooleanOption.get) {
      val ifBlock = node.nodes(1)
      interpret(ifBlock)
    } else if (node.nodes.size == 3) {
      val elseBlock = node.nodes(2)
      interpret(elseBlock)
    }
  }

}
