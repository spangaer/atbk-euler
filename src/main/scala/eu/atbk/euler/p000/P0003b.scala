package eu.atbk.euler.p000

import scala.annotation.tailrec
import eu.atbk.euler.util.MyMath._

object P0003b {
  def main(args: Array[String]) {
    val in = 600851475143l

    //val limit = sqrt(in) + 1

    println(primeGenerator.takeWhile(_ < 100).toList)

    println(P0003b.primeDecompose(in))
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

}
