package eu.atbk.euler.p00

import scala.annotation.tailrec

object P0004b {
  def main(args: Array[String]) {

    println(seek())

  }

  @tailrec
  def seek(outer: Long = 999, inner: Long = 999, ops: Long = 0, highest: Option[Long] = None): (Long, Long) = {
    if (outer >= 100) {
      val now = outer * inner
      val newOps = ops + 1

      val (nextOuter, newBest) = highest match {
        case s @ Some(h) =>
          if (P0004.isPalindrome(now)) {
            // we have the highest palindrome in this series so break
            (true, Some(Math.max(now, h)))
          } else {
            // break inner when going below current best (next attempts can only be lower)
            // inner loops until outer limit
            (inner <= outer || now <= h, s)
          }
        case None =>
          if (P0004.isPalindrome(now)) {
            (true, Some(now))
          } else {
            (inner <= outer, None)
          }
      }

      if (nextOuter) {
        seek(outer - 1, 999, newOps, newBest)
      } else {
        seek(outer, inner - 1, newOps, newBest)
      }
    } else {
      (ops, highest.get)
    }
  }

}
