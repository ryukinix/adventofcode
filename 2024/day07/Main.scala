import scala.io.Source
import scala.math.{addExact, multiplyExact, pow}

case class CalibrationEquation(
    result: Long,
    values: List[Long]
)

object Main extends App {
  test1()
  test2()
  val partA = sumOfValidEquations(input())
  val partB = sumOfValidEquationsWithConcat(input())
  println(s"part(a) | sumOfValidEquations: ${partA}")
  println(s"part(b) | sumOfValidEquationsWithConcat: ${partB}")

  def sumOfValidEquations(equations: List[CalibrationEquation]): Long = {
    equations
      .filter(checkValidEquation)
      .map(_.result)
      .sum()
  }

  def sumOfValidEquationsWithConcat(equations: List[CalibrationEquation]): Long = {
    equations
      .filter(checkValidEquationWithConcat)
      .map(_.result)
      .sum()
  }

  def checkValidEquation(equation: CalibrationEquation): Boolean = {
    generateEquationsResults(equation.values)
      .exists(_ == equation.result)
  }

  def checkValidEquationWithConcat(equation: CalibrationEquation): Boolean = {
    generateEquationsResults(equation.values, List(addExact, multiplyExact, concat))
      .exists(_ == equation.result)
  }

  def concat(a: Long, b: Long): Long = {
    (a.toString() + b.toString).toLong
  }

  def generateEquationsResults(
      values: List[Long],
      ops: List[(Long, Long) => Long] = List(addExact, multiplyExact)
  ): List[Long] = {
    val n            = values.length - 1
    val permutations = permutationsWithRepetition(ops.indices.toList, n).map(0 :: _)
    permutations.map { c =>
      values.zipWithIndex.foldLeft(0.toLong) { case (acc, (x, i)) =>
        ops(c(i))(acc, x)
      }
    }.toList
  }

  def permutationsWithRepetition[T](list: List[T], length: Int): List[List[T]] = {
    if (length == 0) {
      List(Nil)
    } else {
      for {
        element <- list
        perm    <- permutationsWithRepetition(list, length - 1)
      } yield element :: perm
    }
  }

  def test1() = {
    val expectedResult = 3749
    val testResult     = sumOfValidEquations(inputTest())
    val status         = if (testResult == expectedResult) "passed" else "failed"
    println(s"test1[${status}]: got ${testResult}, expected ${expectedResult}")
  }

  def test2() = {
    val expectedResult = 11387
    val testResult     = sumOfValidEquationsWithConcat(inputTest())
    val status         = if (testResult == expectedResult) "passed" else "failed"
    println(s"test2[${status}]: got ${testResult}, expected ${expectedResult}")
  }

  def parseCalibrationEquation(line: String): CalibrationEquation = {
    line.split(":") match {
      case Array(result, numbers) =>
        CalibrationEquation(
          result = result.toLong,
          values = numbers.strip().split(" ").map(_.toLong).toList
        )
      case x => throw new Exception(s"parseCalibrationEquation: failed to parse: {x}")
    }
  }

  def input(): List[CalibrationEquation] = {
    val fileName = "input.txt"
    Source.fromFile(fileName).getLines().toList.map(parseCalibrationEquation)
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
