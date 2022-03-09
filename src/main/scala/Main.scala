import scala.io.{Source, StdIn}

object Main extends App {
  print("Enter the absolute path of the text file: ")
  val pathToFile = StdIn.readLine()
  val bufferedSource = Source.fromFile(pathToFile)
  for (line <- bufferedSource.getLines) {
    println(line.toUpperCase)
  }
  bufferedSource.close
}
