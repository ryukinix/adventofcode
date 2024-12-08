import scala.io.Source
import scala.collection.mutable.HashMap

class Guard(var point: (Int, Int), var direction: (Int, Int)) {
  final val UP    = (-1, 0)
  final val RIGHT = (0, 1)
  final val LEFT  = (0, -1)
  final val DOWN  = (1, 0)

  def rotateDirection(): Unit = {
    this.direction = this.direction match {
      case UP    => RIGHT
      case RIGHT => DOWN
      case LEFT  => UP
      case DOWN  => LEFT
      case _     => this.direction
    }
  }

  def directionChar(): Char = {
    this.direction match {
      case UP    => '^'
      case RIGHT => '>'
      case LEFT  => '<'
      case DOWN  => 'v'
      case _     => '.'
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
  println(matrix2String(generateGuardPathMap(inputTest())))
  test1()
  test2()
  val partA = countDistinctPointsGuardPathMap(parseInput())
  val partB = countObstaclesThatGeneratesLoops(parseInput())
  println(s"part(a) | ${partA}")
  println(s"part(b) | ${partB}")

  def countDistinctPointsGuardPathMap(matrix: Array[Array[Char]]): Int = {
    generateGuardPathMap(matrix).flatten.sum()
  }

  def countObstaclesThatGeneratesLoops(matrix: Array[Array[Char]]): Int = {
    val guard   = initGuard(matrix)
    val pathMap = generateGuardPathMap(matrix)
    val indices: Seq[(Int, Int)] = for {
      rowIndex <- pathMap.indices
      colIndex <- pathMap(rowIndex).indices
      if (rowIndex, colIndex) != guard.point
    } yield (rowIndex, colIndex)

    indices.filter { case (x, y) =>
      pathMap(x)(y) == 1
    }.count(checkLoopByInsertingObstacleAtPoint(matrix.map(_.clone), _))
  }

  def checkLoopByInsertingObstacleAtPoint(matrix: Array[Array[Char]], point: (Int, Int)): Boolean = {
    val (px, py) = point
    matrix(px)(py) = '#'
    checkGuardPathLoop(matrix)
  }

  def test1() = {
    val expectedResult = 41
    val testResult     = countDistinctPointsGuardPathMap(inputTest())
    val status         = if (testResult == expectedResult) "passed" else "failed"
    println(s"test1[${status}]: got ${testResult}, expected ${expectedResult}")
  }

  def test2() = {
    val expectedResult = 6
    val testResult     = countObstaclesThatGeneratesLoops(inputTest())
    val status         = if (testResult == expectedResult) "passed" else "failed"
    println(s"test2[${status}]: got ${testResult}, expected ${expectedResult}")
  }

  def generateGuardPathMap(matrix: Array[Array[Char]]): Array[Array[Int]] = {
    val guard: Guard = initGuard(matrix)
    val (rows, cols) = (matrix.size, matrix(0).size)
    val pathMap      = Array.ofDim[Int](rows, cols)
    var walkPointer  = safeAccess(matrix, guard.point)
    while (walkPointer != None) {
      val (px, py) = guard.point
      pathMap(px)(py) = 1
      walkPointer = safeAccess(matrix, guard.nextPoint())
      walkPointer match {
        case Some('#') => {
          guard.rotateDirection()
        }
        case otherwise => {
          guard.step()
        }
      }
    }
    pathMap
  }

  def checkGuardPathLoop(matrix: Array[Array[Char]]): Boolean = {
    val walkMemory: HashMap[(Int, Int), Array[Char]] = HashMap()
    val guard: Guard                                 = initGuard(matrix)
    var walkPointer                                  = safeAccess(matrix, guard.point)
    while (walkPointer != None) {
      walkPointer = safeAccess(matrix, guard.nextPoint())
      walkPointer match {
        case Some('#') => {
          val directionChar = guard.directionChar()
          // stop if loop is detected
          walkMemory.get(guard.point) match {
            case Some(points) => {
              if (points.contains(directionChar)) {
                return true
              }
            }
            case None =>
          }

          // update walk
          walkMemory.updateWith(guard.point) {
            case Some(points) => Some(points :+ directionChar)
            case None =>  Some(Array(directionChar))
          }
          guard.rotateDirection()
        }
        case otherwise => {
          guard.step()
        }
      }
    }
    // println(matrix2String(matrix))
    return false
  }

  def initGuard(matrix: Array[Array[Char]]): Guard = {
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

  def parseInput(): Array[Array[Char]] = {
    val fileName = "input.txt"
    Source.fromFile(fileName).getLines().toArray().map(_.toArray)
  }

  def safeAccess(matrix: Array[Array[Char]], point: (Int, Int)): Option[Char] = {
    val (line, char) = point
    matrix.lift(line) match {
      case Some(x) => x.lift(char)
      case None    => None
    }
  }

  def inputTest(): Array[Array[Char]] = {
    """....#.....
      |.........#
      |..........
      |..#.......
      |.......#..
      |..........
      |.#..^.....
      |........#.
      |#.........
      |......#...""".stripMargin.split("\n").map(_.toArray)
  }

  def matrix2String[T](matrix: Array[Array[T]]): String =
    matrix.map(_.mkString("")).mkString("\n")
}
