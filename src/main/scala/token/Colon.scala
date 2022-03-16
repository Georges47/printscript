package token

case class Colon(from: Int, to: Int) extends Token {
  override def value: String = ":"
}
