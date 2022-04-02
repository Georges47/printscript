package token

case class LexicalRange(startColumn: Int, endColumn: Int, startLine: Int, endLine: Int) {
  override def toString: String = {
    s" startCol: $startColumn. EndCol: $endColumn. StartLine: $startLine. EndLine: $endLine"
  }
}