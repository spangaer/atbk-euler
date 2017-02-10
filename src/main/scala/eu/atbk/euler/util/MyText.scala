package eu.atbk.euler.util

import scala.collection.immutable.{ Seq => ISeq }

object MyText {
  def isPalindrome(in: String): Boolean = {
    val digits = in.to[ISeq]
    val out = digits == digits.reverse
    out
  }

  val digits9 = (1 to 9).to[ISeq]

  def isPanDigtal9(in: String): Boolean = {
    val digits = in.map(x => Integer.parseInt("" + x)).to[ISeq].sorted
    digits == digits9
  }
}
