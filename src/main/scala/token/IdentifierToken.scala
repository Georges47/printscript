package token

case class IdentifierToken(value: String, from: Int, to: Int, lexicalRange: LexicalRange) extends Token
