import scala.util.matching.Regex

object Day1 {

  def replaceNumberLiterals(line:String):String = {
    var s = ""
    line.foreach(c => {
      s = s.appended(c)
        .replaceFirst("one","1")
        .replaceFirst("two","2")
        .replaceFirst("three","3")
        .replaceFirst("four","4")
        .replaceFirst("five","5")
        .replaceFirst("six","6")
        .replaceFirst("seven","7")
        .replaceFirst("eight","8")
        .replaceFirst("nine","9")
    })
    s
  }

  def replaceReverseNumberLiterals(line:String):String = {
    var s = ""
    line.foreach(c => {
      s = s.appended(c)
        .replaceFirst("one".reverse,"1")
        .replaceFirst("two".reverse,"2")
        .replaceFirst("three".reverse,"3")
        .replaceFirst("four".reverse,"4")
        .replaceFirst("five".reverse,"5")
        .replaceFirst("six".reverse,"6")
        .replaceFirst("seven".reverse,"7")
        .replaceFirst("eight".reverse,"8")
        .replaceFirst("nine".reverse,"9")
    })
    s
  }

  def main(args: Array[String]): Unit = {
    // read the input
    val source = scala.io.Source.fromFile("src/main/resources/day01_input.txt")

    //single digit regex
    val digitPattern:Regex = "[0-9]".r
    try{
      val listOfCodes = source.mkString.split('\n').toList

      // task1
      val task1Sum = listOfCodes.map(line => {
        val first = digitPattern.findFirstIn(line)
        val last = digitPattern.findFirstIn(line.reverse)

        val maybeNumber = for {
          f <- first
          l <- last
        } yield s"$f$l".toInt

        // assume input always has at least one digit
        maybeNumber.get
      }).sum
      println(s"task01 : $task1Sum") //54573

      val task2Sum =  listOfCodes.map(line => {
        val first = digitPattern.findFirstIn(replaceNumberLiterals(line))
        val last = digitPattern.findFirstIn(replaceReverseNumberLiterals(line.reverse))

        val maybeNumber = for {
          f <- first
          l <- last
        } yield s"$f$l".toInt

        // assume input always has at least one digit
        maybeNumber.get
      }).sum
      println(s"task02: $task2Sum") //54591
    }
  }
}
