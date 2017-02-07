package eu.atbk.euler.p000

import scala.BigInt
import scala.math.BigInt.int2bigInt
import eu.atbk.euler.util.MyMath

object P0002 {
  def main(args: Array[String]) {

    val x = MyMath.fibGenerator
      .map { case (a, b) => a }
      .filter { _ % 2 == 0 }
      .takeWhile(_ < 4 * 1000 * 1000)
      .sum

    println(x)
  }
}
