import utils.Utils.readLines

object D01_Trebuchet {
  private type Index = Int

  private val numbersMap: Map[String, Int] = Map(
    "zero" -> 0,
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
  )

  private def indexesOf(
      line: String,
      toCheck: List[String],
      getValue: String => Int
  )(implicit numbersMap: Map[String, Int] = numbersMap): List[(Index, Int)] = toCheck
    .flatMap(number =>
      List(
        (line.toCharArray.indexOfSlice(number), getValue(number)),
        (line.toCharArray.lastIndexOfSlice(number), getValue(number))
      )
    )
    .filter(_._1 >= 0)

  def main(args: Array[String]): Unit = {
    val lines = readLines("01-trebuchet.txt")
    val numbers = lines
      .map(_.toCharArray.filter(_.isDigit).toList)
      .map(digits => (digits.head.toString, digits.last.toString))
      .map(pair => (pair._1 + pair._2).toInt)
    val numbersWithLetters = lines
      .map(line =>
        indexesOf(line, numbersMap.keySet.toList, numbersMap(_)) ++
          indexesOf(line, numbersMap.values.toList.map(_.toString), _.toInt)
      )
      .map(line => (line.minBy(_._1)._2.toString, line.maxBy(_._1)._2.toString))
      .map(pair => (pair._1 + pair._2).toInt)

    println(numbers.sum)
    print(numbersWithLetters.sum)
  }
}
