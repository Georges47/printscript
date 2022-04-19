package interpreter

/**
 * Holds identifiers declared/assigned in the program
 */
case class IdentifierTable() {
  private var identifiers = Map[String, (Option[String], String)]()

  def add(name: String, dataType: String): Map[String, (Option[String], String)] = {
    identifiers += name -> (None, dataType)
    identifiers
  }

  def add(name: String, value: String, dataType: String): Map[String, (Option[String], String)] = {
    identifiers += name -> (Some(value), dataType)
    identifiers
  }

  def value(name: String): Option[String] = {
    identifiers(name)._1
  }

  def dataType(name: String): String = {
    identifiers(name)._2
  }

  def check(name: String): Boolean = {
    identifiers.contains(name)
  }

  def check(name: String, value: String, dataType: String): Boolean = {
    identifiers.contains(name) && identifiers(name) == (Some(value), dataType)
  }

  override def toString: String = {
    identifiers.toList.map(identifier => s"name: ${identifier._1}, value: ${identifier._2._1.getOrElse("-")}, dataType: ${identifier._2._2}").mkString("\n")
  }

}
