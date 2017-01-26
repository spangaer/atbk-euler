package eu.atbk.euler.p00

import scala.annotation.tailrec

object P0003b {
  def main(args: Array[String]) {
    val in = 600851475143l

    //val limit = sqrt(in) + 1

    println(primeGenerator.takeWhile(_ < 100).toList)

    println(primeDecompose(in))
  }

  @tailrec
  def primeDecompose(number: Long, current: Seq[Long] = Seq.empty, now: Long = 2, primes: Iterator[Long] = primeGenerator): Seq[Long] = {

    if (number == now) // the rest is prime
      current :+ number
    else {
      val div = number / now
      val rem = number - (now * div)
      if (rem == 0) {
        primeDecompose(div, current :+ now, now, primes)
      } else {
        primeDecompose(number, current, primes.next(), primes)
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
