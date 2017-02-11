package eu.atbk.euler.p005

import eu.atbk.euler.util.MyText
import scala.annotation.tailrec

object P0055 {
  def main(args: Array[String]) {
    println(lychrelish(196)) // true
    println(lychrelish(47)) // false
    println(lychrelish(10677)) // fake true

    val lychrels = (1 until 10000).count(lychrelish)

    println(lychrels) //249
  }

  private def lychrelish(in: Int): Boolean = {

    @tailrec
    def lychrelish(number: BigInt, iterations: Int): Boolean = {

      val other = BigInt(number.toString().reverse.mkString)
      val next = number + other

      if (MyText.isPalindrome(next.toString())) {
        false
      } else if (iterations <= 0) {
        true
      } else {
        lychrelish(next, iterations - 1)
      }
    }

    lychrelish(BigInt(in), 50)
  }
}
