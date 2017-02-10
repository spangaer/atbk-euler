package eu.atbk.euler.p005

import eu.atbk.euler.util.MyMath

object P0052 {
  def main(args: Array[String]) {
    val res = MyMath.iterator(1).filter(hasProperty).next()
    println(res) // 142857
  }

  private def hasProperty(in: Long): Boolean = {
    val testDigits = MyMath.numberToDigitSeq(in).sorted
    (2 to 6).map { p => p * in }
      .map(MyMath.numberToDigitSeq)
      .map(_.sorted)
      .forall(_ == testDigits)
  }
}
