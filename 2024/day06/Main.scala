import scala.io.Source

class Guard(var point: (Int, Int), var direction: (Int, Int)) {
  def rotateDirection(): Unit = {
    this.direction = this.direction match {
      case (1, 0)  => (0, -1)
      case (0, -1) => (-1, 0)
      case (-1, 0) => (0, 1)
      case (0, 1)  => (1, 0)
      case _       => this.direction
    }
  }

  def nextPoint(): (Int, Int) = {
    val (dx, dy) = this.direction
    val (px, py) = this.point
    (px + dx, py + dy)
  }

  def step(): Unit = {
    val (dx, dy) = this.direction
    val (px, py) = this.point
    this.point = nextPoint()
  }

  override def toString(): String = {
    s"Guard(point=${this.point}, direction=${this.direction})"
  }
}

object Main extends App {
  inputTest().map(println)
  println(matrix2String(generateGuardPathMap(inputTest())))
  test1()
  val partA = countDistinctPointsGuardPathMap(parseInput())
  println(s"part(a) | ${partA}")

  def countDistinctPointsGuardPathMap(matrix: Array[String]): Int = {
    generateGuardPathMap(matrix).flatten.sum()
  }

  def test1() = {
    val expectedResult = 41
    val testResult     = countDistinctPointsGuardPathMap(inputTest())
    val status         = if (testResult == expectedResult) "passed" else "failed"
    println(s"test1[${status}]: got ${testResult}, expected ${expectedResult}")
  }

  def generateGuardPathMap(matrix: Array[String]): Array[Array[Int]] = {
    val guard: Guard = initGuard(matrix)
    val (rows, cols) = (matrix.size, matrix(0).size)
    val pathMap = Array.ofDim[Int](rows, cols)
    var walkPointer = safeAccess(matrix, guard.point)
    while (walkPointer != None) {
      val (px, py) = guard.point
      pathMap(px)(py) = 1
      walkPointer = safeAccess(matrix, guard.nextPoint())
      walkPointer match {
        case Some('#') => {
          guard.rotateDirection()
          guard.step()
        }
        case otherwise => {
          guard.step()
        }
      }
    }
    pathMap
  }

  def initGuard(matrix: Array[String]): Guard = {
    val indices: Seq[(Int, Int)] = for {
      rowIndex <- matrix.indices
      colIndex <- matrix(rowIndex).indices
    } yield (rowIndex, colIndex)
    val point = indices.filter { case (x, y) =>
      matrix(x)(y) == '^'
    }.head

    Guard(
      point = point,
      direction = (-1, 0)
    )
  }

  def parseInput(): Array[String] = {
    val fileName = "input.txt"
    Source.fromFile(fileName).getLines().toArray()
  }

  def safeAccess(matrix: Array[String], point: (Int, Int)): Option[Char] = {
    val (line, char) = point
    matrix.lift(line) match {
      case Some(x) => x.lift(char)
      case None    => None
    }
  }

  def inputTest(): Array[String] = {
    """....#.....
      |.........#
      |..........
      |..#.......
      |.......#..
      |..........
      |.#..^.....
      |........#.
      |#.........
      |......#...""".stripMargin.split("\n")
  }

  def matrix2String[T](matrix: Array[Array[T]]): String =
    matrix.map(_.mkString("")).mkString("\n")
}
