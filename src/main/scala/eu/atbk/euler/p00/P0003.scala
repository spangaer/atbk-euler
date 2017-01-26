package eu.atbk.euler.p00

import scala.annotation.tailrec

object P0003 {
  def main(args: Array[String]) {
    val in = 600851475143l

    //val limit = sqrt(in) + 1

    println(primeGenerator.takeWhile(_ < 100).toList)

    println(primeDecompose(in))
  }

  def sqrt(number: BigInt): BigInt = {
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
  def primeDecompose(number: Long, limit: Long, current: Seq[Long] = Seq.empty, start: Long = 2, primes: Iterator[Long] = primeGenerator): Seq[Long] = {

    if (limit < start) // the rest is prime
      current :+ number
    else {
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

  def primeGenerator: Iterator[Long] = {
    Stream
      .iterate((2l, Seq(2l))) {
        case (now, primes) =>
          val next = now + 1
          if (primes.exists(next % _ == 0)) {
            // not a prime
            (next, primes)
          } else {
            // prime
            (next, primes :+ next)
          }
      }
      .filter {
        case (a, b) =>
          // only keep primes in sequence
          b.last == a
      }
      .map(_._1) // don't need to drag along state
      .iterator
  }
}
