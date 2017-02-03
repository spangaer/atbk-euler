package eu.atbk.euler.p002

import eu.atbk.euler.util.MyMath

object P0020 {
  def main(args: Array[String]) {

    println(solve(10)) // 27

    println(solve(100)) //648

  }

  def solve(i: Long): Long = {
    val fac = MyMath.fac(i)

    fac.toString.toCharArray().map { c => java.lang.Long.parseLong("" + c) }.sum
  }
}
