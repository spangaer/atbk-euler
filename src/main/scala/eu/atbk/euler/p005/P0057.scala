package eu.atbk.euler.p005

import eu.atbk.euler.util.Fraction
import eu.atbk.euler.util.Fraction.Implicits._

object P0057 {
  def main(args: Array[String]) {

    // test fractions
    val f1_2 = Fraction(1, 2)
    val f2_3 = Fraction(2, 3)

    println(f1_2)
    println(f2_3)
    println("")
    println(f1_2 + f2_3)
    println((f1_2 + f2_3).simplify)
    println("")
    println(f1_2 - f2_3)
    println("")
    println(f1_2 * f2_3)
    println((f1_2 * f2_3).simplify)
    println("")
    println(f1_2 / f2_3)
    println("")

    // check first iterations
    iterate.take(9).foreach(println(_))

    val res = iterate.takeWhile(_._1 <= 1000).count {
      case (a, b) =>
        b.numerator.toString().length() > b.denominator.toString().length()
    }

    println(res) //153
  }

  private def next(prevous: Fraction): Fraction = {
    1 + 1 / (1 + prevous)
  }

  private def iterate: Iterator[(Long, Fraction)] = {
    Stream.iterate((0l, Fraction(1, 1))) {
      case (index, current) =>
        (index + 1, next(current).simplify)
    }.iterator
  }
}
