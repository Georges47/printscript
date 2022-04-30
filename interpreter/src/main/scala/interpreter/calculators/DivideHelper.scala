package interpreter.calculators

case class DivideHelper() extends CalculatorHelper {

  override def calculateInts(operand1: String, operand2: String): String = {
    divide(operand1, operand2)
  }

  override def calculateDoubles(operand1: String, operand2: String): String = {
    divide(operand1, operand2)
  }

  override def calculateStrings(operand1: String, operand2: String): String = {
    throw new Exception("Invalid operator applied to string value")
  }

  private def divide(operand1: String, operand2: String): String = {
    String.format("%.2f", operand1.toDouble / operand2.toDouble)
  }

}



