package interpreter.calculators

case class AddHelper() extends CalculatorHelper {

  override def calculateInts(operand1: String, operand2: String): String = {
    (operand1.toInt + operand2.toInt).toString
  }

  override def calculateDoubles(operand1: String, operand2: String): String = {
    String.format("%.2f", operand1.toDouble + operand2.toDouble)
  }

  override def calculateStrings(operand1: String, operand2: String): String = {
    concatenate(operand1, operand2) 
  }

  private def concatenate(string1: String, string2: String): String = {
    string1.replaceAll("^\"|\"$", "") + string2.replaceAll("^\"|\"$", "")
  }
}
