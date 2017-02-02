package eu.atbk.euler.p000

import eu.atbk.euler.util.MyMath

object P0007 {
  def main(args: Array[String]) {

    println(P0007(6))

    val start = System.currentTimeMillis()
    println(P0007(10001))
    val end = System.currentTimeMillis()
    println(end - start)

  }

  def apply(i: Int): Long = {
    MyMath.primeGenerator.drop(i - 1).next()
  }
}
