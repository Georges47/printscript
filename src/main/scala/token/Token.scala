package token

trait Token {
  def value: String
  def from: Int
  def to: Int
  def lexicalRange: LexicalRange
}
