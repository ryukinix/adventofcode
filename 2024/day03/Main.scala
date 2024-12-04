import scala.io.Source

// definition of the function pipeline operator: |>
extension [A](a: A)
  inline def |>[B](inline f: A => B): B = f(a)

object Main extends App {
  val input = inputProgram()
  val test = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
  val testResult = parse(test)
  val partA = parse(input)
  val partB = input |> eraseDisabledSections |> parse
  println(s"test: got $testResult, expected 161")
  println(s"part(a) | parse: $partA")
  println(s"part(b) | parseWithouDisabledInstructions: $partB")

  def inputProgram(): String = {
    val fileName = "input.txt"
    Source.fromFile(fileName).getLines().mkString
  }

  def parse(program: String): Int = {
    val regex = """mul\((\d+),(\d+)\)""".r
    regex.findAllIn(program).map {
      case regex(a, b) => a.toInt * b.toInt
    }.sum()
  }

  def eraseDisabledSections(program: String): String = {
    val regex = """don't\(\).*?(do\(\)|$)""".r
    regex.replaceAllIn(program, "*")
  }
}
