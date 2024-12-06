import scala.io.Source

case class PageData(
  pageOrderingRules: List[(Int, Int)],
  pageNumbers: List[List[Int]]
)


object Main extends App {

  // val input = parseInput()
  println(inputTest())

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
      pageNumbers = pageNumbers
    )
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
