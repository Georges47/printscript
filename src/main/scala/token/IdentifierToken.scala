package token

case class IdentifierToken(value: String, from: Int, to: Int) extends Token
