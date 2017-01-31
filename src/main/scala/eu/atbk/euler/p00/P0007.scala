package eu.atbk.euler.p00

import eu.atbk.euler.util.MyMath

object P0007 {
  def main(args: Array[String]) {

    println(P0007(6))

    println(P0007(10001))
  }

  def apply(i: Int): Long = {
    MyMath.primeGenerator.drop(i - 1).next()
  }
}
