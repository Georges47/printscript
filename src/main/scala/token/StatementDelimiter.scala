package token

case class StatementDelimiter(from: Int, to: Int, lexicalRange: LexicalRange) extends Token {
  override def value: String = ";"
}