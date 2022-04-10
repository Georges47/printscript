package interpreter

/**
 * Holds all the variables declared/assigned in the program
 */
case class VariableTable() {
  private var variables = Map[String, (Option[String], String)]()

  def add(name: String, dataType: String): Map[String, (Option[String], String)] = {
    variables += name -> (None, dataType)
    variables
  }

  def add(name: String, value: String, dataType: String): Map[String, (Option[String], String)] = {
    variables += name -> (Some(value), dataType)
    variables
  }

  def value(name: String): Option[String] = {
    variables(name)._1
  }

  def dataType(name: String): String = {
    variables(name)._2
  }

  def check(name: String): Boolean = {
    variables.contains(name)
  }

  def check(name: String, value: String, dataType: String): Boolean = {
    variables.contains(name) && variables(name) == (Some(value), dataType)
  }

}
