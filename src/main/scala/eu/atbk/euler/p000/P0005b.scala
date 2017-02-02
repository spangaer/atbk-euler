package eu.atbk.euler.p000

import eu.atbk.euler.util.MyMath
import scala.collection.immutable.{ Seq => ISeq }

object P0005b {
  def main(args: Array[String]) {

    P0005b(10)
    //2520

    P0005b(20)
    //232792560
  }

  def apply(l: Int) {
    val devidors = P0005b.devidors(l)
    println(devidors.product)
  }

  def devidors(l: Int): Seq[Long] = {
    val devidorsStart = (1 to l)

    val decomposed = devidorsStart.map(MyMath.primeDecompose(_))
    val primeFactors = MyMath.primeGenerator.takeWhile(_ <= l).to[ISeq]

    // for each decomposition, get the max occurences of a prime number
    val maxPowers = primeFactors.map(prime => decomposed.map(_.filter(_ == prime).size).max)

    primeFactors.zip(maxPowers).map {
      case (prime, power) =>
        Stream.fill(power)(prime).product
    }
  }
}
