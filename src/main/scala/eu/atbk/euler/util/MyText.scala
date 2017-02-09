package eu.atbk.euler.util

import scala.collection.immutable.{ Seq => ISeq }

object MyText {
  def isPalindrome(in: String): Boolean = {
    val digits = in.to[ISeq]
    val out = digits == digits.reverse
    out
  }
}
