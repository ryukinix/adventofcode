import scala.io.Source
import scala.math.abs

object Main extends App {
  val input = inputPairNumbers()
  val distancesSum = minimumPairDistanceSum(input)

  println(s"Input[0:10]: ${input.slice(0, 10)} ...")
  println(s"distanceSum: ${distancesSum}")

  def inputPairNumbers(): List[(Int, Int)] = {
    val fileName = "input.txt"
    Source.fromFile(fileName).getLines().toList.map { line =>
      line.split("\\s+").map(_.toInt) match {
        case Array(first, second) => (first, second)
      }
    }
  }

  def minimumPairDistanceSum(pairNums: List[(Int, Int)]): Int = {
    val first: List[Int] = pairNums.map(_._1).sorted().toList
    val second: List[Int] = pairNums.map(_._2).sorted().toList
    val distances: List[Int] = first.zip(second).map { case (x, y) =>
      abs(x - y)
    }
    distances.sum()
  }

}
