package eu.atbk.euler.util

import scala.annotation.tailrec
import scala.collection.GenSeq

object MyMath {

  def primeGenerator: Iterator[Long] = {
    Stream
      .iterate((2l, Seq(2l): GenSeq[Long], false)) {
        case (now, primes, stop) =>
          val next = now + 1
          if (primes.takeWhile { p =>
            val sq = p * p
            sq > 0 && sq <= next
          }.exists(next % _ == 0)) {
            // not a prime
            (next, primes, stop)
          } else {
            // prime
            (next, primes :+ next, next * next < 0)
          }
      }
      .takeWhile {
        case (_, _, stop) =>
          !stop
      }
      .filter {
        case (a, b, _) =>
          // only keep primes in sequence
          b.last == a
      }
      .map(_._1) // don't need to drag along state
      .iterator
  }

  private def sqrt(number: BigInt): BigInt = {
    def next(n: BigInt, i: BigInt): BigInt = (n + i / n) >> 1
    val one = BigInt(1)
    val n = one
    val n1 = next(n, number)
    def sqrtHelper(n: BigInt, n1: BigInt): BigInt = if ((n1 - n).abs <= one) List(n1, n).max else sqrtHelper(n1, next(n1, number))
    sqrtHelper(n, n1)
  }

  def sqrt(number: Long): Long =
    sqrt(BigInt(number)).longValue()

  def primeDecompose(number: Long): Seq[Long] =
    primeDecompose(number, sqrt(number) + 1)

  @tailrec
  private def primeDecompose(number: Long, limit: Long, current: Seq[Long] = Seq.empty, start: Long = 2, primes: Iterator[Long] = primeGenerator): Seq[Long] = {

    if (limit < start) { // the rest is prime
      if (number != 1)
        current :+ number
      else
        current
    } else {
      val div = number / start
      val rem = number - (start * div)
      if (rem == 0) {
        // 
        val limit = sqrt(div) + 1
        primeDecompose(div, limit, current :+ start, start, primes)
      } else {
        primeDecompose(number, limit, current, primes.next(), primes)
      }
    }
  }
}
