package token

case class AssignmentOperator(from: Int, to: Int, lexicalRange: LexicalRange) extends Token {
  override def value: String = "="
}
