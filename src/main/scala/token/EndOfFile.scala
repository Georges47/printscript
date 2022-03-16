package token

case class EndOfFile(from: Int, to: Int) extends Token {
  override def value: String = "EOF"
}
