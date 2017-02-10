package eu.atbk.euler.p004

import eu.atbk.euler.util.MyMath

object P0047 {
  def main(args: Array[String]) {

    println(solve(2)) // 14
    println(solve(3)) // 644
    println(solve(4)) // 134043

  }

  def solve(n: Int): Seq[(Long, Seq[Long])] = {
    MyMath.iterator(10)
      .map(i => (i, MyMath.primeDecompose10to12th(i).distinct))
      .sliding(n, 1)
      .find(_.forall(_._2.size == n)).get
  }
}
