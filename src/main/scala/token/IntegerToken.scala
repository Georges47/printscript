package token

case class IntegerToken(value: String, from: Int, to: Int, lexicalRange: LexicalRange) extends Token
