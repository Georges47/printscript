package parser

import abstractSyntaxTree.AbstractSyntaxTree
import org.austral.ingsis.printscript.common.{LexicalRange, Token}
import org.austral.ingsis.printscript.parser.{Content, TokenIterator}
import token.TokenConsumerImpl
import token.types._

import scala.collection.JavaConverters.seqAsJavaListConverter
import scala.collection.mutable.ListBuffer

class Parser {
  def parse(fileContent: String, tokens: List[Token]): AbstractSyntaxTree = {
    val abstractSyntaxTree: ListBuffer[AbstractSyntaxTree] = ListBuffer.empty
    val tokenIterator = TokenIterator.create(fileContent, tokens.asJava)
    val tokenConsumer = TokenConsumerImpl(tokenIterator)
    while (tokenConsumer.current.getType != EndOfFile) {
      val ast = consumeTokens(tokenConsumer)
      if(ast.isDefined) abstractSyntaxTree += ast.get
    }
    AbstractSyntaxTree(new Content("Program", new Token(Program, 0, 0, new LexicalRange(0, 0, 0, 0))), abstractSyntaxTree.toList)
  }

  def consumeTokens(tokenConsumer: TokenConsumerImpl): Option[AbstractSyntaxTree] = {
    val currentToken = tokenConsumer.current
    currentToken.getType match {
      case Let => parseVariableDeclarationAssignation(tokenConsumer)
      case Identifier => parseIdentifier(tokenConsumer)
      case Println => parsePrintln(tokenConsumer)
      case NumberValue | StringValue | OpenParenthesis =>
        Some(parseExpression(tokenConsumer))
      case Newline =>
        tokenConsumer.consume(currentToken.getType); None
      case _ =>
        tokenConsumer.consume(currentToken.getType)
        throw new Exception("Unknown token")
    }
  }

  def parseExpression(tokenConsumer: TokenConsumerImpl): AbstractSyntaxTree = {
    val abstractSyntaxTree: ListBuffer[AbstractSyntaxTree] = ListBuffer.empty
    parseExpressionHelper(tokenConsumer, abstractSyntaxTree, 0, 0)
  }

  // Si hay igual numero de parentesis abiertos que de parentesis cerrados AND lo que le sigue al parentesis cerrado no es un operador, chao
  def parseExpressionHelper(tokenConsumer: TokenConsumerImpl, currentASTs: ListBuffer[AbstractSyntaxTree], numberOfOpenParenthesis: Int, numberOfClosedParenthesis: Int): AbstractSyntaxTree = {
    val previousContent = tokenConsumer.consumeAny(NumberValue, StringValue, Identifier, OpenParenthesis, ClosedParenthesis, Plus, Minus, Asterisk, FrontSlash)

//    else {
//      currentASTs += AbstractSyntaxTree(previousContent)
//    }
    currentASTs += AbstractSyntaxTree(previousContent)
    val currentToken = tokenConsumer.current
//    println("")
//    println(tokenConsumer.tokenIterator.current())
    val nextToken = tokenConsumer.peek(2)(1).getToken
//    println(tokenConsumer.tokenIterator.current())

    if (currentToken.getType == ClosedParenthesis &&
        numberOfOpenParenthesis == numberOfClosedParenthesis &&
        (nextToken.getType != Plus || nextToken.getType != Minus || nextToken.getType != Asterisk || nextToken.getType != FrontSlash)) {
      return AbstractSyntaxTree(new Content("Expression", emptyToken), currentASTs.toList)
    }

//    if (numberOfOpenParenthesis == numberOfClosedParenthesis &&
//        (currentToken.getType != Plus || currentToken.getType != Minus || currentToken.getType != Asterisk || currentToken.getType != FrontSlash)) {
////      currentASTs += parseExpression(tokenConsumer)
//      AbstractSyntaxTree(new Content("Expression", emptyToken), currentASTs.toList)
//    }

    currentToken.getType match {
      case NumberValue | StringValue | Identifier => parseExpressionHelper(tokenConsumer, currentASTs, numberOfOpenParenthesis, numberOfClosedParenthesis)
      case Plus | Minus | Asterisk | FrontSlash => parseExpressionHelper(tokenConsumer, currentASTs, numberOfOpenParenthesis, numberOfClosedParenthesis)
      case OpenParenthesis => parseExpressionHelper(tokenConsumer, currentASTs, numberOfOpenParenthesis + 1, numberOfClosedParenthesis)
      case ClosedParenthesis => parseExpressionHelper(tokenConsumer, currentASTs, numberOfOpenParenthesis, numberOfClosedParenthesis + 1)
//      case Semicolon | ClosedParenthesis =>
      case Semicolon  =>
//        tokenConsumer.consumeAny(Semicolon, ClosedParenthesis)
        tokenConsumer.consumeAny(Semicolon)
        AbstractSyntaxTree(new Content("Expression", emptyToken), currentASTs.toList)
    }

  }

  def parseIdentifier(tokenConsumer: TokenConsumerImpl): Option[AbstractSyntaxTree] = {
    val nextTwoTokens: List[Content[String]] = tokenConsumer.peek(2)
    val identifier = nextTwoTokens.head.getToken //.consume(Identifier)
    val nextToken = nextTwoTokens(1).getToken //tokenConsumer.consumeAny(Assignment, Plus, Minus, Asterisk, FrontSlash, Semicolon).getToken

    nextToken.getType match {
      case Assignment => parseIdentifierAssignment(tokenConsumer)
      case Plus | Minus | Asterisk | FrontSlash => Some(parseExpression(tokenConsumer))
      case Semicolon => None
    }
  }

  def parseIdentifierAssignment(tokenConsumer: TokenConsumerImpl): Option[AbstractSyntaxTree] = {
    val identifier = tokenConsumer.consume(Identifier)
    tokenConsumer.consume(Assignment)
    val expression = parseExpression(tokenConsumer)
    if (tokenConsumer.current.getType == Semicolon) tokenConsumer.consume(Semicolon)
    Some(AbstractSyntaxTree(new Content("Assignment", emptyToken), List(AbstractSyntaxTree(identifier), expression)))
  }

  def parseVariableDeclarationAssignation(
      tokenConsumer: TokenConsumerImpl
  ): Option[AbstractSyntaxTree] = {
    tokenConsumer.consume(Let)
    val identifier = tokenConsumer.consume(Identifier)
    tokenConsumer.consume(Colon)
    val dataType = tokenConsumer.consumeAny(StringDataType, NumberDataType)

    val currentToken = tokenConsumer.current
    currentToken.getType match {
      case Assignment =>
        tokenConsumer.consume(Assignment)
        val expression = parseExpression(tokenConsumer)
//        val expression = tokenConsumer.consumeAny( // Aqui deberia ser donde consume una expresion
//          StringValue,
//          NumberValue
//        )
        if (tokenConsumer.current.getType == Semicolon) tokenConsumer.consume(Semicolon)
        Some(
          AbstractSyntaxTree(
            new Content("DeclarationAndAssignment", emptyToken),
            List(
              AbstractSyntaxTree(identifier),
              AbstractSyntaxTree(dataType),
              expression
            )
          )
        )
      case Semicolon =>
        tokenConsumer.consume(Semicolon)
        Some(
          AbstractSyntaxTree(
            new Content("Declaration", emptyToken),
            List(
              AbstractSyntaxTree(identifier),
              AbstractSyntaxTree(dataType)
            )
          )
        )
      case _ =>
        throw new Exception(
          s"Expected semicolon at line ${currentToken.getRange.getEndLine}, column ${currentToken.getRange.getEndCol}"
        )
    }
  }

  def parsePrintln(tokenConsumer: TokenConsumerImpl): Option[AbstractSyntaxTree] = {
    tokenConsumer.consume(Println)
    tokenConsumer.consume(OpenParenthesis)
//    val value = tokenConsumer.consumeAny(StringValue, NumberValue, Identifier) // Expression
    val expression = parseExpression(tokenConsumer)
    tokenConsumer.consume(ClosedParenthesis)

    val currentToken = tokenConsumer.current
    currentToken.getType match {
      case Semicolon =>
        tokenConsumer.consume(Semicolon)
//      case Newline =>
//        tokenConsumer.consume(Newline)
        Some(
          AbstractSyntaxTree(
            new Content("Println", emptyToken),
            List(expression)
          )
        )
      case _ =>
        throw new Exception(
          s"Expected semicolon at line ${currentToken.getRange.getEndLine}, column ${currentToken.getRange.getEndCol}"
        )
    }
  }

  def emptyToken: Token = {
    new Token(Empty, 0, 0, new LexicalRange(0, 0, 0, 0))
  }

}
