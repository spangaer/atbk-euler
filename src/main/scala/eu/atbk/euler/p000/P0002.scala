package eu.atbk.euler.p000

import scala.BigInt
import scala.math.BigInt.int2bigInt

object P0002 {
  def main(args: Array[String]) {

    val start = (BigInt(1), BigInt(1))
    val x = Stream.iterate(start) { case (a, b) => (b, a + b) }
      .map { case (a, b) => b }
      .filter { _ % 2 == 0 }
      .takeWhile(_ < 4 * 1000 * 1000)
      .sum

    println(x)
  }
}
