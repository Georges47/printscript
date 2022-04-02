package parser

import abstractSyntaxTree.AbstractSyntaxTree
import token.{AssignmentOperator, Colon, EndOfFile, IdentifierToken, StatementDelimiter, Token}

import scala.collection.mutable.ListBuffer

abstract class Expression
case class IntegerExpression ( value: Int ) extends Expression
case class StringExpression ( value: String ) extends Expression
case class VariableExpression ( name: String ) extends Expression
case class BinaryExpression ( operator: String, left: Expression, right: Expression ) extends Expression
case class UnaryExpression ( operator: String, operand: Expression ) extends Expression
case class CallExpression ( name: String, arguments: List[Expression] ) extends Expression
case class ProjectionExpression ( record: Expression, attribute: String ) extends Expression
case class RecordExpression ( arguments: List[(String,Expression)] ) extends Expression

abstract class Statement
case class AssignmentStatement(literal: Token, dataType: Token, value: Token) extends Statement
case class DeclarationStatement(identifier: Token, dataType: Token) extends Statement

class Parser {
  def parse(tokens: Iterator[Token]): AbstractSyntaxTree = {
    val abstractSyntaxTree: ListBuffer[AbstractSyntaxTree] = ListBuffer.empty
    while (tokens.hasNext) {
      abstractSyntaxTree += consumeTokens(tokens)
    }
  AbstractSyntaxTree("Program", abstractSyntaxTree.toList)
  }

  def consumeTokens(tokens: Iterator[Token]): AbstractSyntaxTree = {
    readToken(tokens) match {
      case None => AbstractSyntaxTree() //EmptyNode
      case Some(token) => token match {
        case token if token.value == "let" => parseLiteralDeclaration(tokens)
        case EndOfFile(_, _, _) => AbstractSyntaxTree("EndOfFile")
        case _ => AbstractSyntaxTree("AAA") //; throw new Exception("Unknown token")
      }
    }
  }

  def readToken(iterator: Iterator[Token]): Option[Token] = {
    if (iterator.hasNext) Some(iterator.next) else None
  }

  def parseLiteralDeclaration(iterator: Iterator[Token]): AbstractSyntaxTree = {
    val nameToken = readToken(iterator)
    val colon = readToken(iterator)
    val dataTypeToken = readToken(iterator)
    val nextToken = readToken(iterator)

    (nameToken, colon, dataTypeToken) match {
      case (Some(nameToken), Some(colon), Some(dataTypeToken)) => (nameToken, colon, dataTypeToken) match {
        case (IdentifierToken(_, _, _, _), Colon(_, _, _), IdentifierToken(_, _, _, _)) => nextToken match {
          case Some(token) => token match {
            case StatementDelimiter(_, _, _) => AbstractSyntaxTree("VariableDeclaration", List(AbstractSyntaxTree(dataTypeToken.value), AbstractSyntaxTree(nameToken.value))) // DeclarationStatement(nameToken, dataTypeToken)
            case AssignmentOperator(_, _, _) => readToken(iterator) match {
              case None => throw new Exception("Malformed declaration/assignment, no name for identifier")
              case Some(valueToken) => readToken(iterator) match {
                case Some(StatementDelimiter(_, _, _)) => AbstractSyntaxTree("VariableDeclaration", List(AbstractSyntaxTree(dataTypeToken.value), AbstractSyntaxTree(nameToken.value), AbstractSyntaxTree(valueToken.value)))
                case _ => throw new Exception("Malformed declaration/assignment, no name for identifier")
              }
            }
            case _ => throw new Exception("Malformed declaration/assignment, no name for identifier")
          }
          case _ => throw new Exception("Malformed declaration/assignment, no name for identifier")
        }
        case _ => throw new Exception("Malformed declaration/assignment, no name for identifier")
      }
      case _ => throw new Exception("Malformed declaration/assignment, no name for identifier")
    }
  }
}
