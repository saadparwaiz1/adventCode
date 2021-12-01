package advent.day1

import scala.io.Source

case class Day1(src: String) {
  def getData: List[Int] = {
    val file = Source.fromFile(src)
    val lines = file.getLines().map(_.toInt).toList
    file.close()
    lines
  }

  def countIncreasing: Int = {
    val data = this.getData
    data.sliding(2).foldLeft(0)((count, win) => count + (if(win.head < win.tail.head) 1 else 0))
  }

  def countIncSlide: Int = {
    val data = this.getData
    data.sliding(3).map(_.sum).sliding(2).foldLeft(0)((count, win) => count + (if(win.head < win.tail.head) 1 else 0))
  }
}

object Day1 {
  def main(args: Array[String]): Unit = {
    val day = new Day1("src/main/Resources/input.txt")
    println(s"part 1: ${day.countIncreasing}")
    println(s"part 2: ${day.countIncSlide}")
  }
}