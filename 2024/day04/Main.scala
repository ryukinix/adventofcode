import scala.io.Source

object Main extends App {
  inputTest().map(println)
  test1(); test2()

  val input = inputCrossWords()
  val partA = crossSearchKeyword(input, "XMAS")
  val partB = crossSearchKeywordInX(input, "MAS")
  println(s"partA | crossSearchKeyword(XMAS): ${partA}")
  println(s"partB | crossSearchKeywordInX(MAS): ${partB}")

  def test1() = {
    val expectedResult = 18
    val testResult     = crossSearchKeyword(inputTest(), "XMAS")
    val status         = if (testResult == expectedResult) "passed" else "failed"
    println(s"test1[${status}]: got ${testResult}, expected ${expectedResult}")

  }

  def test2() = {
    val expectedResult = 9
    val testResult     = crossSearchKeywordInX(inputTest(), "MAS")
    val status         = if (testResult == expectedResult) "passed" else "failed"
    println(s"test2[${status}]: got ${testResult}, expected ${expectedResult}")

  }

  def crossSearchKeyword(crossWords: Array[String], keyword: String): Int = {
    val indices: Seq[(Int, Int)] = for {
      rowIndex <- crossWords.indices
      colIndex <- crossWords(rowIndex).indices
    } yield (rowIndex, colIndex)
    indices.filter { case (x, y) => crossWords(x)(y) == keyword.head }
      .map(p => crossSearchKeywordAtPoint(crossWords, keyword, p))
      .sum()
  }

  def crossSearchKeywordInX(crossWords: Array[String], keyword: String): Int = {
    val indices: Seq[(Int, Int)] = for {
      rowIndex <- crossWords.indices
      colIndex <- crossWords(rowIndex).indices
    } yield (rowIndex, colIndex)
    val middleIndex = keyword.size / 2
    val middleChar  = keyword(middleIndex)
    indices.filter { case (x, y) => crossWords(x)(y) == middleChar }
      .count(p => crossSearchKeywordInXAtPoint(crossWords, keyword, p))
  }

  private def crossSearchKeywordInXAtPoint(crossWords: Array[String], keyword: String, point: (Int, Int)): Boolean = {
    val directions: Seq[(Int, Int)] = for {
      rowIndex <- Seq(1, -1)
      colIndex <- Seq(1, -1)
    } yield (rowIndex, colIndex)
    val m        = keyword.size / 2
    val (px, py) = point
    directions.count { case d @ (dx, dy) =>
      crossSearchKeywordFromDirection(crossWords, keyword, (px - dx * m, py - dy * m), d)
    }.equals(2)
  }

  private def crossSearchKeywordAtPoint(crossWords: Array[String], keyword: String, point: (Int, Int)): Int = {
    val directions: Seq[(Int, Int)] = for {
      rowIndex <- Seq(0, 1, -1)
      colIndex <- Seq(0, 1, -1)
      if (rowIndex, colIndex) != (0, 0)
    } yield (rowIndex, colIndex)

    directions.count(d => crossSearchKeywordFromDirection(crossWords, keyword, point, d))
  }

  private def crossSearchKeywordFromDirection(
      crossWords: Array[String],
      keyword: String,
      point: (Int, Int),
      direction: (Int, Int)
  ): Boolean = {
    val (px, py) = point
    val (dx, dy) = direction
    keyword.zipWithIndex.forall { (char, i) =>
      safeAccess(crossWords, (px + i * dx, py + i * dy)) == Some(char)
    }
  }

  private def safeAccess(crossWords: Array[String], point: (Int, Int)): Option[Char] = {
    val (line, char) = point
    crossWords.lift(line) match {
      case Some(x) => x.lift(char)
      case None    => None
    }
  }

  private def inputCrossWords(): Array[String] = {
    val fileName = "input.txt"
    Source.fromFile(fileName).getLines().toArray()
  }

  private def inputTest(): Array[String] = {
    """MMMSXXMASM
      |MSAMXMSMSA
      |AMXSXMAAMM
      |MSAMASMSMX
      |XMASAMXAMM
      |XXAMMXXAMA
      |SMSMSASXSS
      |SAXAMASAAA
      |MAMMMXMMMM
      |MXMXAXMASX""".stripMargin.split("\n")
  }
}
