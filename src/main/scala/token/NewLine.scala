package token

case class NewLine(from: Int, to: Int, lexicalRange: LexicalRange) extends Token {
  override def value: String = "\n"
}
