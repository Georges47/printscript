package token

case class DoubleToken(value: String, from: Int, to: Int, lexicalRange: LexicalRange) extends Token
