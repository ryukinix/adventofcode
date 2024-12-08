import scala.collection.mutable.HashMap
import scala.math.{pow, sqrt}
import scala.io.Source

type Point = (Int, Int)

object Main extends App {
  println(generateHashMapOfAntennas(inputTest()))
  test1()
  val partA = countAntiNodes(input())
  println(s"part(a) | countAntiNodes: ${partA}")

  def countAntiNodes(matrix: Array[Array[Char]]): Int = {
    val mapOfAntennas = generateHashMapOfAntennas(matrix)
    mapOfAntennas.values
      .map(generateAntiNodes)
      .flatten()
      .toSet
      .count(checkValidAntiNode(matrix, _))
  }

  def checkValidAntiNode(matrix: Array[Array[Char]], point: Point): Boolean = {
    safeAccess(matrix, point) match {
      case Some(x)   => true
      case otherwise => false
    }
  }

  def generateAntiNodes(points: Set[Point]): Set[Point] = {
    points.map { p1 =>
      (points - p1).map(p2 => generateAntiNode(p1, p2))
    }.flatten()
  }

  def generateAntiNode(p1: Point, p2: Point): Point = {
    val (x1, y1) = p1
    val (x2, y2) = p2
    val (dx, dy) = (x2 - x1, y2 - y1)
    (x1 - dx, y1 - dy)
  }

  def generateHashMapOfAntennas(matrix: Array[Array[Char]]): HashMap[Char, Set[Point]] = {
    val mapOfAntennas: HashMap[Char, Set[Point]] = HashMap()
    val indices: Seq[(Int, Int)] = for {
      rowIndex <- matrix.indices
      colIndex <- matrix(rowIndex).indices
    } yield (rowIndex, colIndex)

    indices.foreach {
      case point @ (px, py) => {
        matrix(px)(py) match {
          case '.' =>
          case c =>
            mapOfAntennas.updateWith(c) {
              case Some(points) => Some(points + point)
              case None         => Some(Set(point))
            }
        }
      }
    }
    mapOfAntennas
  }

  def safeAccess(matrix: Array[Array[Char]], point: Point): Option[Char] = {
    val (line, char) = point
    matrix.lift(line) match {
      case Some(x) => x.lift(char)
      case None    => None
    }
  }

  def input(): Array[Array[Char]] = {
    Source.fromFile("input.txt").getLines().map(_.toArray).toArray
  }

  def test1() = {
    val expectedResult = 14
    val testResult     = countAntiNodes(inputTest())
    val status         = if (testResult == expectedResult) "passed" else "failed"
    println(s"test1[${status}]: got ${testResult}, expected ${expectedResult}")
  }

  def inputTest(): Array[Array[Char]] = {
    """............
      |........0...
      |.....0......
      |.......0....
      |....0.......
      |......A.....
      |............
      |............
      |........A...
      |.........A..
      |............
      |............""".stripMargin.split("\n").map(_.toArray)
  }

  def matrix2String[T](matrix: Array[Array[T]]): String =
    matrix.map(_.mkString("")).mkString("\n")

}
