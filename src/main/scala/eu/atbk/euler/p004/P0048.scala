package eu.atbk.euler.p004

import eu.atbk.euler.util.MyMath

object P0048 {
  def main(args: Array[String]) {

    println(solve(10)) // 405071317

    println(solve(1000)) // 9110846700
  }

  def solve(n: Int): Long = {
    val prod = digitRetainingProduct(10)
    val sum = (1 to n).map { i =>
      val x = Stream.fill(i)(i).map(_.toLong).reduce(prod)
      //      println(x)
      x
    }.sum

    // trim sum
    prod(1, sum)
  }

  private def digitRetainingProduct(digits: Int): (Long, Long) => Long = {
    val mask = MyMath.power(10, digits)

    (a, b) =>
      {
        val aa = if (a > mask) a % mask else a
        val bb = if (b > mask) b % mask else b
        val c = aa * bb
        val cc = if (c > mask) c % mask else c
        cc
      }

  }
}
