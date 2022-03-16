package token

case class AssignmentOperator(from: Int, to: Int) extends Token {
  override def value: String = "="
}
