import scala.util.matching.Regex

object Day3 {


  def main(args: Array[String]): Unit = {

    // read the input
    val source = scala.io.Source.fromFile("src/main/resources/day03_input.txt")
    try {
      val regex = "[0-9]{1,3}".r
      val gameLines = source.mkString.split('\n').toList

//      println(gameLines(1))
      val MIN = 0
      val MAX = gameLines.head.length-1

      val ranges = (line:String,reg:Regex) => reg.findAllMatchIn(line).map(s =>(s.start,s.end)).toList
      val allRanges = gameLines.map(l => ranges(l,regex))
      val ss = allRanges.zipWithIndex.map {
        case(item,index) => {
          val prev = gameLines(Math.max(0,index-1))
          val now = gameLines(index)
          val next = gameLines(Math.min(gameLines.length-1,index+1))
          isSymbolInrange(item,List(prev,now,next),MAX)
        }
      }
      println(ss.flatten.sum)

      // task 2
      val starRegex = """\*""".r


      val starRanges = gameLines.map(l => ranges(l,starRegex))

      val starLines = starRanges.zipWithIndex.map {
        case(item,index) => {
          val prev = allRanges(Math.max(0,index-1))
          val now = allRanges(index)
          val next = allRanges(Math.min(gameLines.length-1,index+1))
          isGearRatio(item,List(prev,now,next),List(
            gameLines(Math.max(0,index-1)),gameLines(index),gameLines(Math.min(gameLines.length-1,index+1))
          ),MAX)
        }
      }
      println(starLines.flatten.sum)

    }finally {
      source.close()
    }

  }

  def isGearRatio(starRange:List[(Int,Int)],numRanges:List[List[(Int,Int)]],numStrings:List[String],max:Int): List[Int] = {
    starRange.map({
      case (start, end) => {
        val candidates = numRanges.zipWithIndex.flatMap {
          case (item, index) => {
            item.map({
              case (numStart, numEnd) => {
                val starStart = Math.max(start - 1, 0)
                val starENd = Math.min(max, end + 1)
                val check = Range(starStart, starENd) intersect Range(numStart, numEnd)
                if (check.nonEmpty) {
                  Some(numStrings(index).substring(numStart, numEnd).toInt)
                } else {
                  None
                }
              }
            })
          }
        }.filter(_.isDefined).map(_.get)
        if (candidates.length == 2) {
          candidates.product
        } else {
          0
        }
      }
    })
  }
  def isSymbolInrange(range:List[(Int,Int)],lines:List[String],max:Int):List[Int] = {
    val symbol ="[^0-9.]".r

   range.map({
      case(start,end) => {
        val tempS = lines.map(l => l.substring(Math.max(start - 1, 0), Math.min(end+1, max))).mkString
        if(symbol.findFirstIn(tempS).isDefined){
          lines(1).substring(start,end).toInt
        }else{
          0
        }
      }
    })
  }
}
//544433
//76314915
