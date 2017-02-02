package eu.atbk.euler.p000

import scala.annotation.tailrec
import eu.atbk.euler.util.MyMath._

object P0003 {
  def main(args: Array[String]) {
    val in = 600851475143l

    //val limit = sqrt(in) + 1

    println(primeGenerator.takeWhile(_ < 100).toList)

    println(primeDecompose(in))
  }
}
