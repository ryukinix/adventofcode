import scala.io.Source

object Main extends App {
  val input = inputReadLines()
  val test = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
  val testResult = parseLine(test)
  val partA = parseLines(input)
  println(s"test: got $testResult, expected 161")
  println(s"part(a) | parseLines: $partA")

  def inputReadLines(): List[String] = {
    val fileName = "input.txt"
    Source.fromFile(fileName).getLines().toList
  }

  def parseLine(line: String): Int = {
    val regex = """mul\((\d+),(\d+)\)""".r
    regex.findAllIn(line).map {
      case regex(a, b) => a.toInt * b.toInt
    }.sum()
  }

  def parseLines(lines: List[String]): Int = {
    lines.map(parseLine).sum()
  }
}
