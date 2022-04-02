package token

case class StringToken(value: String, from: Int, to: Int, lexicalRange: LexicalRange) extends Token
