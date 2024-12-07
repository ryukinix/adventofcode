import scala.io.Source
import scala.math.{addExact, multiplyExact, pow}


case class CalibrationEquation(
  result: Long,
  values: List[Long]
)

object Main extends App {
  test1()
  val partA = sumOfValidEquations(parseInput())
  println(s"part(a) | sumOfValidEquations: ${partA}")

  def sumOfValidEquations(equations: List[CalibrationEquation]): Long = {
    equations
      .filter(checkValidEquation)
      .map(_.result)
      .sum()
  }

  def checkValidEquation(equation: CalibrationEquation): Boolean = {
    generateEquationsResults(equation.values)
      .exists(_ == equation.result)
  }

  def generateEquationsResults(values: List[Long]): List[Long] = {
    val ops: List[(Long, Long) => Long] = List(addExact, multiplyExact)
    val n = values.length - 1
    val maxBinary = (pow(2, n))
    val combinations = (0 until maxBinary.toInt).map { k =>
      0 :: integerToBinaryWithLeadingZeros(k, n) //.map(ops(_))
    }
    combinations.map { c =>
      values.zipWithIndex.foldLeft(c.head.toLong) {
        case (acc, (x, i)) => ops(c(i))(acc, x)
      }
    }.toList
  }

  def integerToBinaryWithLeadingZeros(number: Int, length: Int): List[Int] = {
    // Convert the number to a binary string
    val binaryString = number.toBinaryString

    // Calculate the number of leading zeros needed
    val leadingZeros = length - binaryString.length

    // Create a formatted string with leading zeros
    val formattedBinaryString = "0" * leadingZeros + binaryString
    formattedBinaryString.map(_.asDigit).toList
  }

  def test1() = {
    val expectedResult = 3749
    val testResult     = sumOfValidEquations(inputTest())
    val status         = if (testResult == expectedResult) "passed" else "failed"
    println(s"test1[${status}]: got ${testResult}, expected ${expectedResult}")
  }

  def parseInput(): List[CalibrationEquation] = {
    input().map(parseCalibrationEquation)
  }

  def parseCalibrationEquation(line: String): CalibrationEquation = {
    line.split(":") match {
      case Array(result, numbers) => CalibrationEquation(
        result = result.toLong,
        values = numbers.strip().split(" ").map(_.toLong).toList
      )
      case x => throw new Exception(s"parseCalibrationEquation: failed to parse: {x}")
    }
  }

  def input(): List[String] = {
    val fileName = "input.txt"
    Source.fromFile(fileName).getLines().toList
  }

  def inputTest(): List[CalibrationEquation] = {
    """190: 10 19
      |3267: 81 40 27
      |83: 17 5
      |156: 15 6
      |7290: 6 8 6 15
      |161011: 16 10 13
      |192: 17 8 14
      |21037: 9 7 18 13
      |292: 11 6 16 20""".stripMargin.split("\n").map(parseCalibrationEquation).toList
  }
}
