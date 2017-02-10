package eu.atbk.euler.p004

import eu.atbk.euler.util.MyMath
import scala.collection.immutable.{ Seq => ISeq }
import scala.collection.immutable.{ HashSet => IHashSet }

object P0049 {
  def main(args: Array[String]) {
    val dataset = MyMath.primesBelow1million.dropWhile(_ < 1000).takeWhile(_ < 10000)

    val lookup = dataset.to[IHashSet]

    val res = dataset.foldLeft(ISeq.empty[ISeq[Long]]) {
      case (seq, p) =>
        if (seq.exists(_.contains(p))) {
          seq
        } else {
          val permPrimes = MyMath.numberToDigitSeq(p).permutations
            .map(MyMath.digitSeqToNumber)
            .filterNot(MyMath.numberToDigitSeq(_).size != 4)
            .filter(MyMath.isPrimeBelow10to12th).to[ISeq].sorted

          if (permPrimes.size >= 3) {
            seq :+ permPrimes
          } else
            seq
        }

    } // we have permutations that are primes, now apply equidistance property to combo's of 3
      .flatMap { seq =>
        seq.combinations(3)
      }.filter(seq => seq(1) - seq(0) == seq(2) - seq(1))

    println(res)
    res.foreach(x => println(x.mkString))
    // 148748178147
    // 296962999629
  }
}
