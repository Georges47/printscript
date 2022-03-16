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
//  /*
//    // let x = 2; =>
//
//      binary expressions
//      declaration and assignment
//      assignment => LiteralToken, AssignmentOperator, ..., StatementDelimiter
//      agarrar todo lo que esta entre el AssignmentOperator y el StatementDelimiter y meterlo en una Expression
//   */
//
  def getAbstractSyntaxTree(tokens: BufferedIterator[Token]): AbstractSyntaxTree = {
    val abstractSyntaxTree: ListBuffer[AbstractSyntaxTree] = ListBuffer.empty
    while (tokens.hasNext) {
      abstractSyntaxTree += consumeTokens(tokens)
    }
  AbstractSyntaxTree("Program", abstractSyntaxTree.toList)
  }

  def consumeTokens(tokens: BufferedIterator[Token]): AbstractSyntaxTree = {
    readToken(tokens) match {
      case None => AbstractSyntaxTree() //EmptyNode
      case Some(token) => token match {
        case token if token.value == "let" => parseLiteralDeclarationOrAssignment(tokens)
        case EndOfFile(_, _) => AbstractSyntaxTree("EndOfFile")
        case _ => println(token); AbstractSyntaxTree("AAA")//; throw new Exception("Unknown token")
      }
    }
  }

  def readToken(bufferedIterator: BufferedIterator[Token]): Option[Token] = {
    if (bufferedIterator.hasNext) Some(bufferedIterator.next) else None
  }
//
  def parseLiteralDeclarationOrAssignment(bufferedIterator: BufferedIterator[Token]): AbstractSyntaxTree = {
//    // detecto que tengo que entrar aqui cuando veo un "let"
//    bufferedIterator.next
//    // suponiendo que el primer valor es el nombre de la variable
    val nameToken = readToken(bufferedIterator)
//    bufferedIterator.next

    val colon = readToken(bufferedIterator)
//    bufferedIterator.next
    val dataTypeToken = readToken(bufferedIterator)
//    bufferedIterator.next

    val nextToken = readToken(bufferedIterator)
//    bufferedIterator.next

//    println(nameToken)
//    println(colon)
//    println(dataTypeToken)
     println(nextToken)

    (nameToken, colon, dataTypeToken) match {
      case (Some(nameToken), Some(colon), Some(dataTypeToken)) => (nameToken, colon, dataTypeToken) match {
        case (IdentifierToken(_, _, _), Colon(_, _), IdentifierToken(_, _, _)) => nextToken match {
          case None => throw new Exception("Malformed declaration/assignment, no name for identifier")
          case Some(token) => token match {
            case StatementDelimiter(_, _) => AbstractSyntaxTree("VariableDeclaration", List(AbstractSyntaxTree(dataTypeToken.value), AbstractSyntaxTree(nameToken.value))) // DeclarationStatement(nameToken, dataTypeToken)
            case AssignmentOperator(_, _) => readToken(bufferedIterator) match {
              case None => throw new Exception("Malformed declaration/assignment, no name for identifier")
              case Some(valueToken) => AbstractSyntaxTree("VariableDeclaration", List(AbstractSyntaxTree(dataTypeToken.value), AbstractSyntaxTree(nameToken.value), AbstractSyntaxTree(valueToken.value))) // AssignmentStatement(nameToken, dataTypeToken, valueToken)
            }
            case _ => throw new Exception("Malformed declaration/assignment, no name for identifier")
          }
        }
        case _ => throw new Exception("Malformed declaration/assignment, no name for identifier")
      }
      case _ => throw new Exception("Malformed declaration/assignment, no name for identifier")
    }

//    literalToken.get.
//    bufferedIterator.next
//    readToken(bufferedIterator) match {
//      case None => throw new Exception("Non finished declaration or assignment statement")
//      case Some(token) => token match {
//        case StatementDelimiter => DeclarationStatement(literalToken.get)
//        case AssignmentOperator =>
//          if (literalToken.nonEmpty) { bufferedIterator.next; AssignmentStatement(literalToken.get, parseAssignmentHelper(List.empty, bufferedIterator)) }
//          else throw new Exception("No literal specified")
//        case _ => throw new Exception("Non finished declaration or assignment statement")
//      }
//    }
//
  }
//
//  private def parseAssignmentHelper(currentTokens: List[Token], bufferedIterator: BufferedIterator[Token]): Expression = {
//    readToken(bufferedIterator) match {
//      case
//    }
//  }
}
