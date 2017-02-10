package eu.atbk.euler.p005

import eu.atbk.euler.util.MyMath

import scala.collection.immutable.{ Seq => ISeq }
import scala.collection.immutable.{ HashSet => IHashSet }

object P0051 {
  def main(args: Array[String]) {
    val s13 = MyMath.numberToDigitSeq(13)
    val s56003 = MyMath.numberToDigitSeq(56003)

    println(digitReplacedPrimesFamily(s13, Seq(0)))
    println(digitReplacedPrimesFamily(s56003, Seq(2, 3)))
    println(maxNDigitReplacedPrimesFamilies(s13, 1))
    println(maxNDigitReplacedPrimesFamilies(s56003, 2))

    println(solve(6)) // 13
    println(solve(7)) // 56003
    println(solve(8)) // 121313

  }

  private def solve(n: Int): Seq[Long] = {
    val dataset = MyMath.primesBelow1million.dropWhile(_ < 10)

    dataset.iterator.map(maxDigitReplacedPrimesFamilies).dropWhile(_.size < n).next()
  }

  private def maxDigitReplacedPrimesFamilies(in: Long): Seq[Long] = {
    val inDigits = MyMath.numberToDigitSeq(in)
    (1 until (inDigits.size)).map { d =>
      maxNDigitReplacedPrimesFamilies(inDigits, d)
    }.maxBy(_.size)
  }

  private def maxNDigitReplacedPrimesFamilies(in: Seq[Int], digits: Int): Seq[Long] = {
    (0 until in.size).combinations(digits)
      .map(digitReplacedPrimesFamily(in, _))
      .maxBy(_.size)
  }

  private def digitReplacedPrimesFamily(in: Seq[Int], digits: Seq[Int]): Seq[Long] = {
    (0 to 9).map { r =>
      digits.foldLeft(in) {
        case (numberseq, d) =>
          numberseq.updated(d, r)
      }
    }.filterNot(_.head == 0) // don't include leading zeros
      .map(MyMath.digitSeqToNumber)
      .filter(MyMath.isPrimeBelow10to12th)
  }
}
