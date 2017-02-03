package eu.atbk.euler.p001

import scala.collection.immutable.{ Seq => ISeq }
import eu.atbk.euler.util.MyMath

object P0012 {

  def main(args: Array[String]) {
    println(P0012(5))

    println(P0012(500))

  }

  def apply(i: Int): (Long, Long, Seq[Long]) = {
    triangleNumberGenerator.map {
      case (index, sum) =>
        (index, sum, MyMath.primeDecompose(sum))
    }
      .map {
        case (a, b, c) =>
          val d = (1 until c.size).flatMap(i => c.combinations(i)).distinct.map(_.product)
          (a, b, 1l +: d)
      }
      .dropWhile {
        case (a, b, d) =>
          println(s"${(a, b, d.size)}")
          d.size < i
      }
      .next()
  }

  def triangleNumberGenerator: Iterator[(Long, Long)] = {
    Stream.iterate((1l, 1l)) { case (index, sum) => (index + 1, sum + index + 1) }.iterator
  }
}
