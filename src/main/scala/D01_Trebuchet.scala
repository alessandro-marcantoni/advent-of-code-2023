import utils.Utils.readLines

object D01_Trebuchet {
  private val numbersMap: Map[Int, String] = Map(
    1 -> "one",
    2 -> "two",
    3 -> "three",
    4 -> "four",
    5 -> "five",
    6 -> "six",
    7 -> "seven",
    8 -> "eight",
    9 -> "nine"
  )

  private def replaceWords(line: String): String =
    numbersMap.foldLeft(line) { case (acc, (key, value)) =>
      acc.replaceAll(value, value(0) + key.toString + value(value.length - 1))
    }

  private def solve(lines: List[String]): Int = lines
    .map(_.toCharArray.filter(_.isDigit).toList)
    .map(digits => (digits.head.toString, digits.last.toString))
    .map(pair => (pair._1 + pair._2).toInt)
    .sum

  def main(args: Array[String]): Unit = {
    val lines = readLines("01-trebuchet.txt")
    val linesWithWords = lines.map(replaceWords)

    println(solve(lines))
    println(solve(linesWithWords))
  }
}
