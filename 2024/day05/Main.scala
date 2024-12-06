import scala.io.Source
import scala.annotation.tailrec

case class PageData(
  pageOrderingRules: List[(Int, Int)],
  updatePageNumbers: List[List[Int]]
)


object Main extends App {
  // val input = parseInput()
  //println(inputTest())
  test1()
  val input = parseInput()
  val result = sumMiddleNumberOfValidPageNumbers(input)
  println(s"part(a) | sumMiddleNumberOfValidPageNumbers: ${result}")

  @tailrec
  def checkValidPageNumbers(pageOrderingRules: List[(Int, Int)], pageNumbers: List[Int]): Boolean = {
    pageNumbers match {
      case Nil => true
      case head :: tail => {
        tail.forall(n => pageOrderingRules.exists(p => p == (head, n))) && checkValidPageNumbers(pageOrderingRules, tail)
      }
    }
  }

  def sumMiddleNumberOfValidPageNumbers(pageData: PageData): Int = {
    pageData.updatePageNumbers
      .filter {
        pageNumbers => checkValidPageNumbers(pageData.pageOrderingRules, pageNumbers)
      }.map {
        pageNumbers => pageNumbers(pageNumbers.size / 2)
    }.sum()
  }

  def parseInput(): PageData = {
    val fileName = "input.txt"
    parseLines(Source.fromFile(fileName).getLines().toList)
  }

  def parseLines(lines: List[String]): PageData = {
    val endLineIndex = lines.indexOf("")
    val (firstSection, secondSection) = lines.splitAt(endLineIndex)
    val pageOrderingRules = firstSection.map { line =>
      line.split('|') match {
        case Array(x, y) => (x.toInt, y.toInt)
      }
    }
    val pageNumbers = secondSection.tail.map(_.split(",").map(_.toInt).toList).toList
    PageData(
      pageOrderingRules = pageOrderingRules,
      updatePageNumbers = pageNumbers
    )
  }

  def test1() = {
    val expectedResult = 143
    val testResult     = sumMiddleNumberOfValidPageNumbers(inputTest())
    val status         = if (testResult == expectedResult) "passed" else "failed"
    println(s"test1[${status}]: got ${testResult}, expected ${expectedResult}")
  }

  def inputTest(): PageData = {
    val entry = """47|53
                  |97|13
                  |97|61
                  |97|47
                  |75|29
                  |61|13
                  |75|53
                  |29|13
                  |97|29
                  |53|29
                  |61|53
                  |97|53
                  |61|29
                  |47|13
                  |75|47
                  |97|75
                  |47|61
                  |75|61
                  |47|29
                  |75|13
                  |53|13
                  |
                  |75,47,61,53,29
                  |97,61,53,29,13
                  |75,29,13
                  |75,97,47,61,53
                  |61,13,29
                  |97,13,75,29,47""".stripMargin.split("\n").toList
    parseLines(entry)
  }
}
