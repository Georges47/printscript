package interpreter.calculators

trait CalculatorHelper {
  def calculateInts(operand1: String, operand2: String): String
  def calculateDoubles(operand1: String, operand2: String): String
  def calculateStrings(operand1: String, operand2: String): String
}
