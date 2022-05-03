package interpreter.inputs

trait InputProvider {
  def input(name: String): String
}
