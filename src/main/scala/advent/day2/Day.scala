package advent.day2

import scala.io.Source


case class Day(src: String) {
  val output = this.getData

  def getData: List[(String, Int)] = {
    val file = Source.fromFile(src)
    val lines = file.getLines().map(_.split(' ')).toList
    file.close()
    lines.map(x => (x(0), x(1).toInt))
  }

  def predictAimPos(): Int = {
    val pos = output.foldLeft((0, 0, 0))((x, y) => {
      if (y._1 == "forward") {
        (x._1 + y._2, x._2 + x._3 * y._2, x._3)
      } else if (y._1 == "down") {
        (x._1, x._2, x._3 + y._2)
      }
      else {
        (x._1, x._2, x._3 - y._2)
      }
    })
    pos._1 * pos._2
  }

  def predictPos(): Int = {
    val pos = output.foldLeft((0, 0))((x, y) => {
      if (y._1 == "forward") {
        (x._1 + y._2, x._2)
      } else if (y._1 == "down") {
        (x._1, x._2 + y._2)
      }
      else {
        (x._1, x._2 - y._2)
      }
    })
    pos._1 * pos._2
  }
}

object Day {
  def main(args: Array[String]): Unit = {
    val day = new Day("src/main/Resources/input2.txt")
    println(s"part 1: ${day.predictPos()}")
    println(s"part 2: ${day.predictAimPos()}")
  }
}
