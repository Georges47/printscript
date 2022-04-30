package interpreter.calculators

case class SubtractHelper() extends CalculatorHelper {

  override def calculateInts(operand1: String, operand2: String): String = {
    (operand1.toInt - operand2.toInt).toString
  }

  override def calculateDoubles(operand1: String, operand2: String): String = {
    String.format("%.2f", operand1.toDouble - operand2.toDouble)
  }

  override def calculateStrings(operand1: String, operand2: String): String = {
    throw new Exception("Invalid operator applied to string value")
  }

}
