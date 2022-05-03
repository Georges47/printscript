package interpreter.inputs

class ConsoleInputProvider() extends InputProvider {
  override def input(value: String): String = {
    scala.io.StdIn.readLine()
  }
}
