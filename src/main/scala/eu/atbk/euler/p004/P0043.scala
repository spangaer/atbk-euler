package eu.atbk.euler.p004

import eu.atbk.euler.util.MyMath

import scala.collection.immutable.{ Seq => ISeq }

object P0043 {
  def main(args: Array[String]) {
    println(primes)
    println(hasProperty("1406357289".map(i => Integer.parseInt("" + i))))
    println(Long.MaxValue)
    val res = (0 to 9).permutations.to[ISeq].par.filter(hasProperty).map(MyMath.digitSeqToNumber).to[ISeq]

    println(res)
    println(res.size) // 6
    println(res.sum) // 16695334890
  }

  private val primes = MyMath.primeGenerator.takeWhile(_ <= 17).to[ISeq]

  private def hasProperty(in: Seq[Int]): Boolean = {
    in.drop(1).sliding(3).map(MyMath.digitSeqToNumber(_).toInt)
      .zip(primes.iterator).forall {
        case (a, p) =>
          a % p == 0
      }
  }
}
