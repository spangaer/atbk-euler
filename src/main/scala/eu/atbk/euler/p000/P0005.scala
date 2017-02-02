package eu.atbk.euler.p000

import eu.atbk.euler.util.MyMath.primeGenerator

object P0005 {
  def main(args: Array[String]) {

    P0005(10)
    //2520

    P0005(20)
    //232792560
  }

  def apply(i: Int) {
    val devidors = (1 to i)

    println(devidors)

    val res = Stream.iterate(i + 1l)(_ + 1).filter(i => devidors.forall(i % _ == 0)).apply(0)

    println(res)
  }
}
