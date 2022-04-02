package token

case class OperatorToken(value: String, from: Int, to: Int, lexicalRange: LexicalRange) extends Token
