package token

case class KeywordToken(value: String, from: Int, to: Int, lexicalRange: LexicalRange) extends Token
