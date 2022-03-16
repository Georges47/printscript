package token

case class StatementDelimiter(from: Int, to: Int) extends Token {
  override def value: String = ";"
}