import scala.io.Source

object Main extends App {
  val input = inputEngineMatrix()
  println(matrix2String(buildMaskMatrix(input)))

  def buildMaskMatrix(engineMatrix: Array[Array[Char]]): Array[Array[Int]] = {
    for {
      line <- engineMatrix
    } yield {
      line.map {
        char => char match {
          case c if c.isDigit => 1
          case '.' => 0
          case otherwise => 2
        }
      }
    }
  }

  def buildValidDigitsMatrix(maskMatrix: Array[Array[Int]]): Unit = {
    val height = maskMatrix.size
    val width = maskMatrix(0).size
    val contigousOnesPosition = findContiguousOnes(maskMatrix)

    // rest of the solution:
    // for each pair start/end of number, use it to scan neighbors between start:end
    // a valid number need to check each digit:
    //                   200
    //                   010
    //                   000
    // 2 is a symbol. It would be interesting to get a
    // multi-dimensional slicing considering the limits, then, flat
    // it like Array(2, 0, 0, 0, 1, 0, 0, 0, 0) any number 2 is matched,
    // the number is valid!
  }


  def findContiguousOnes(matrix: Array[Array[Int]]): Array[Array[(Int, Int)]] = {
    matrix.map(findContiguousOnesInRow)
  }

  def findContiguousOnesInRow(row: Array[Int]): Array[(Int, Int)] = {
    val contiguousOnes = scala.collection.mutable.ArrayBuffer[(Int, Int)]()
    var start = -1

    for (i <- row.indices) {
      if (row(i) == 1) {
        if (start == -1) {
          start = i
        }
      } else {
        if (start != -1) {
          contiguousOnes.append((start, i - 1))
          start = -1
        }
      }
    }

    // Check if there was a contiguous sequence that ended at the last element
    if (start != -1) {
      contiguousOnes.append((start, row.length - 1))
    }

    contiguousOnes.toArray
  }

  def inputEngineMatrix(): Array[Array[Char]] = {
    val fileName = "input.txt"
    Source.fromFile(fileName).getLines().map(_.toArray).toArray
  }

  def matrix2String[T](matrix: Array[Array[T]]): String =
    matrix.map(_.mkString("")).mkString("\n")
}
