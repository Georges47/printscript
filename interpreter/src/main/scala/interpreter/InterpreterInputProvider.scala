package interpreter

trait InterpreterInputProvider {
  def getInput(value: String): String
}
