import scala.io.Source
import scala.math.{signum, abs}

object Main extends App {
  val numbers = inputNumbers()
  val safeLines = numbers.count(safeLine)
  val safeLinesTolerant = numbers.count(safeLineTolerant)
  // numbers.filter(safeLine).map(println)
  // println(safeLine(List(17, 18, 11, 8, 3)))
  println(s"part(a) | safeLines: ${safeLines}")
  println(s"part(b) | safeLinesTolerant: ${safeLinesTolerant}")

  def inputNumbers(): List[List[Int]] = {
    val fileName = "input.txt"
    Source.fromFile(fileName).getLines()
          .map(_.split("\\s+").map(_.toInt).toList).toList
  }

  def safeLineTolerant(line: List[Int]): Boolean = {
    val linesWithoutOneLevel = line.zipWithIndex.map {
      case (_, idx) => line.patch(idx, Nil, 1)
    }
    linesWithoutOneLevel.exists(safeLine)
  }

  def safeLine(line: List[Int]): Boolean = {
    val checksumTarget = line.size - 1
    val checksum = line.zip(line.tail).map {
      (a, b) => if (abs(a - b) <= 3) signum(a - b) else 0
    }.sum()
    abs(checksum) == checksumTarget
  }
}
