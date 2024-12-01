import scala.io.Source
import scala.math.abs

object Main extends App {
  val input = inputPairNumbers()
  val distancesSum = minimumPairDistanceSum(input)

  println(s"Input[0:10]: ${input.slice(0, 10)} ...\n")
  println(s"part(a) | distanceSum: ${distancesSum}")
  println(s"part(b) | similaritySum: ${similaritySum(input)}")

  def inputPairNumbers(): List[(Int, Int)] = {
    val fileName = "input.txt"
    Source.fromFile(fileName).getLines().toList.map { line =>
      line.split("\\s+").map(_.toInt) match {
        case Array(first, second) => (first, second)
      }
    }
  }

  def similaritySum(pairNums: List[(Int, Int)]): Int = {
    val numbers = pairNums.map(_._2)

    // Using groupBy and mapValues
    val frequencyMap = numbers.groupBy(identity).mapValues(_.size)
    pairNums.map(_._1).map(x => x * frequencyMap.getOrElse(x, 0)).sum()
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
