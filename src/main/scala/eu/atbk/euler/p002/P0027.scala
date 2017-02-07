package eu.atbk.euler.p002

import eu.atbk.euler.util.MyMath

object P0027 {

  def main(args: Array[String]) {

    println(sequentialPrimes(quad(1, 41))) // 40
    println(sequentialPrimes(quad(-79, 1601))) // 80

    val range = (-999 to 999)

    val out = range.flatMap(a => range.map(b => (a, b))).par
      .map { case (a, b) => (a, b, sequentialPrimes(quad(a, b))) }.maxBy(_._3)

    println(s"$out\t${out._1 * out._2}")
  }

  def quad(a: Long, b: Long): (Long => Long) = { x =>
    x * x + a * x + b
  }

  def sequentialPrimes(f: (Long => Long)): Long = {
    Stream.iterate(0)(_ + 1).takeWhile(n => MyMath.isPrime(f(n))).size
  }

}
