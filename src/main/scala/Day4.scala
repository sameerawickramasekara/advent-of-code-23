object Day4 {

  def main(args: Array[String]): Unit = {


    val source = scala.io.Source.fromFile("src/main/resources/day04_input.txt")
    val gameLines = source.mkString.split('\n').toList


    val sum = gameLines.map {
      case s"$id:$winningString|$ourString" => Some(id, winningString, ourString)
      case _ => None
    }.map(_.get)
      .map { case (_, w, o) => {
        val winningNumbers = w.trim.split(" ").map(_.trim).filter(_.nonEmpty).toSet
        val ourNumbers = o.trim.split(" ").map(_.trim).filter(_.nonEmpty).toSet

        val overlap = winningNumbers intersect ourNumbers

        val res = if (overlap.nonEmpty) Math.pow(2, overlap.size - 1) else 0
        res
      }
      }.sum

    println(sum.toInt)

    val originalWinnings = gameLines.map {
      case s"$id:$winningString|$ourString" => Some(id, winningString, ourString)
      case _ => None
    }.map(_.get)
      .map { case (l, w, o) => {
        val winningNumbers = w.trim.split(" ").map(_.trim).filter(_.nonEmpty).toSet
        val ourNumbers = o.trim.split(" ").map(_.trim).filter(_.nonEmpty).toSet

        val number = l.split("d")(1).trim

        val overlap = winningNumbers intersect ourNumbers
        (number.toInt -> overlap.size)

      }
      }.toMap

    def scratch(number: Int, winning: List[Int]): List[Int] = {

      println(s"processing line $number")
      if (number == originalWinnings.size + 1) {
        return winning
      }
      val taking = winning.filter(p => p == number).appended(number)
      //    val remaining = winning.filter(p => p != number)
      val temp = winning.appended(number)
      val additions = originalWinnings.get(number)

      //      println(taking.flatMap(i =>  (number+1 to Math.min(number+additions.get,originalWinnings.size+1)).toList))
      val res = temp.appendedAll(taking.flatMap(i => (number + 1 to Math.min(number + additions.get, originalWinnings.size + 1)).toList))
      scratch(number + 1, res)

    }

    println(scratch(1, List.empty).size)
    //7258152

  }

}
