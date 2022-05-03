package interpreter

class ConsoleInputProvider() extends InterpreterInputProvider {
  override def getInput(value: String): String = {
    scala.io.StdIn.readLine()
  }
}
