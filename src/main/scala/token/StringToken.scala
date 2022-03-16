package token

case class StringToken(value: String, from: Int, to: Int) extends Token
