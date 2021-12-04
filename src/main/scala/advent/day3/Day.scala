package advent.day3

import scala.annotation.tailrec
import scala.io.Source


case class Day(src: String) {
  val output: List[Array[Int]] = this.getData

  def getData: List[Array[Int]] = {
    val file = Source.fromFile(src)
    val lines = file.getLines().map(_.toCharArray).toList
    file.close()
    lines.map(_.map(_.toInt - 48))
  }

  private def getGamma(sumArray: Array[Int], totalElements: Int): Array[Int] = {
    val gamma = sumArray.map(x => if(x >= totalElements-x) 1 else 0)
    gamma
  }
  private def getEpislon(sumArray: Array[Int], totalElements: Int): Array[Int] = {
    val epislon = sumArray.map(x => if(x < totalElements-x) 1 else 0)
    epislon
  }

  private def getSumArray(output: List[Array[Int]]): Array[Int] = {
    val default: Array[Int] = output.head.map(_ => 0)
    val reduced: Array[Int] = output.foldLeft(default)((a, b) => {
      a.zip(b).map { case (x, y) => x + y }
    })
    reduced
  }

  def calcPower(): Int = {
    val sumArray = getSumArray(output)
    val gamma = getGamma(sumArray, output.length)
    val epislon = getEpislon(sumArray, output.length)
    Integer.parseInt(gamma.mkString(""), 2) * Integer.parseInt(epislon.mkString(""), 2)
  }

  @tailrec
  private def reducedFilter(arr: List[Array[Int]], currIndex: Int = 0, filterFunc: (Array[Int], Int) => Array[Int]): Array[Int] = {
    if(arr.length == 1){
      arr.head
    }
    else {
      val gamma = filterFunc(getSumArray(arr), arr.length)
      reducedFilter(arr.filter(x => x(currIndex) == gamma(currIndex)), currIndex+1, filterFunc)
    }
  }

  private def getOxygen(): Int = {
    val reducedList = reducedFilter(output, 0, getGamma).mkString("")
    Integer.parseInt(reducedList, 2)
  }

  private def getCarbon(): Int = {
    val reducedList = reducedFilter(output, 0, getEpislon).mkString("")
    Integer.parseInt(reducedList, 2)
  }

  def getLifeSupport(): Int = {
    getCarbon() * getOxygen()
  }

}

object Day {
  def main(args: Array[String]): Unit = {
    val day = Day("src/main/Resources/input3.txt")
    println(s"part 1: ${day.calcPower()}")
    println(s"part 2: ${day.getLifeSupport()}")
  }
}
