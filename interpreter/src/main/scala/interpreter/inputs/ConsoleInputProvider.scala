package interpreter.inputs

class ConsoleInputProvider() extends InputProvider {
  override def input(name: String): String = {
    scala.io.StdIn.readLine()
  }
}
