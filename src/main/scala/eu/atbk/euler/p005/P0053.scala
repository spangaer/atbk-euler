package eu.atbk.euler.p005

import eu.atbk.euler.util.MyMath

object P0053 {
  def main(args: Array[String]) {
    // lazy brute force solution

    println(c(5, 3))

    val limit = BigInt(1000 * 1000)

    val res = (1 to 100).par.flatMap { n =>
      (1 to n).map { r =>
        c(n, r)
      }
    }.count(_ > limit)

    println(res) // 4075
  }

  def c(n: Int, r: Int): BigInt = {
    MyMath.fac(BigInt(n)) / (MyMath.fac(BigInt(r)) * MyMath.fac(BigInt(n - r)))
  }
}
