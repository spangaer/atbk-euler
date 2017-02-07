package eu.atbk.euler.p003

import eu.atbk.euler.util.MyMath

object P0030 {
  def main(args: Array[String]) {
    println(powerDec(1634, 4)) //1634
    println(powerDec(8208, 4)) //8208
    println(powerDec(9474, 4)) //9474
    println(all9(9999)) //true
    println(all9(9998)) //false

    //    (2 to 1000).foreach(x => println(s"$x\t${powerDec(x, 4)}"))

    println(solve(4)) //19316
    println(solve(5)) //443839
  }

  private def solve(n: Int): Long = {
    val boundHelp = MyMath.power(10, n)

    Stream.iterate(2)(_ + 1)
      .map(i => (i, powerDec(i, n)))
      // all 9 is the max sum, for a given number of decimals
      // if that is smaller then actual number + what can be added by an extra decimal
      // summing got out of reach => guestimate bound
      .takeWhile { case (i, pd) => !(all9(i) && pd + boundHelp < i) }
      .filter { case (i, pd) => i == pd }
      .map(_._1)
      .sum
  }

  private def powerDec(in: Long, pow: Int): Long = {
    in.toString().toCharArray()
      .map("" + _)
      .map(java.lang.Long.parseLong(_))
      .map(MyMath.power(_, pow)).sum
  }

  private def all9(in: Long): Boolean = {
    in.toString().toCharArray().filterNot(_ == '9').isEmpty
  }

}
