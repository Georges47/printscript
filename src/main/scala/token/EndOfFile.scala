package token

case class EndOfFile(from: Int, to: Int, lexicalRange: LexicalRange) extends Token {
  override def value: String = "EOF"
}
