package eu.atbk.euler.p003

import eu.atbk.euler.util.MyMath
import scala.collection.immutable.{ HashSet => IHashSet }

object P0037 {
  val lookup = MyMath.primesBelow1million.to[IHashSet]

  def main(args: Array[String]) {

    val dataset = MyMath.primesBelow1million.dropWhile(_ < 10).seq

    val out = dataset.filter(truncatable)

    println(out)
    println(out.size)
    println(out.sum)

  }

  private def truncatable(in: Long): Boolean = {
    val digits = MyMath.numberToDigitSeq(in)

    (1 until digits.size).forall { i =>
      val left = MyMath.digitSeqToNumber(digits.drop(i))
      val right = MyMath.digitSeqToNumber(digits.take(digits.size - i))

      lookup.contains(left) && lookup.contains(right)
    }
  }
}
