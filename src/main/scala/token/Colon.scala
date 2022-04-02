package token

case class Colon(from: Int, to: Int, lexicalRange: LexicalRange) extends Token {
  override def value: String = ":"
}
