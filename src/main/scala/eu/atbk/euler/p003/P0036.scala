package eu.atbk.euler.p003

import eu.atbk.euler.util.MyText

object P0036 {
  def main(args: Array[String]) {

    println(stringify(585)) // (585, 1001001001)

    val out = (1 until (1000 * 1000)).filter { x =>
      val y @ (d, b) = stringify(x)
      if (MyText.isPalindrome(d) && MyText.isPalindrome(b)) {
        println(y)
        true
      } else
        false

    }.map(_.longValue()).sum

    println(out) // 872187
  }

  def stringify(in: Long): (String, String) = {
    val dec = in.toString()
    val bin = in.toBinaryString
    (dec, bin)
  }
}
