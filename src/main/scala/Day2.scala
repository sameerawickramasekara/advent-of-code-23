object Day2 {

  def main(args: Array[String]): Unit = {
    // read the input
    val source = scala.io.Source.fromFile("src/main/resources/day02_input.txt")
    try {
      val gameLines = source.mkString.split('\n')

      val limitMap = Map(
        ("red" -> 12),
        ("green" -> 13),
        ("blue" -> 14),
      )
      val maps = gameLines.map(line => {
        (line.trim.split(':')(0).split(' ')(1).toInt -> line.trim.split(':')(1).split(';').map(attempt => attempt.trim.split(',').map(pair => {
          val colorAndCount = pair.trim.split(' ')
          (colorAndCount(1) -> colorAndCount(0).toInt)
        }).toMap).toList)
      }).toMap

      val sum = maps.filter({
        case (_, value) => value.forall(p => {
          limitMap.keys.forall(lk => {
            p.getOrElse(lk, 0) <= limitMap.getOrElse(lk, 0)
          })
        })
      }).keys.sum

      println(sum)

      // task2
      val s = maps.map({
        case(key,values) => values.flatten.groupBy({
          case (k,v) => k
        })
      }).toList

      val maxEach = s.map(hm => hm.map({
        case (key,value) => (key , value.flatMap(x => List(x._2)).sorted.max)
      }))

      val sum2 = maxEach.map(hm => hm.values.product).sum

      println(sum2)
    } finally {
      source.close()
    }
    // results
    //2176
    //63700
  }
}
