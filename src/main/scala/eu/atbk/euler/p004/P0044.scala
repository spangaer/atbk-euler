package eu.atbk.euler.p004

import scala.collection.immutable.{ Seq => ISeq }
import scala.collection.immutable.{ HashSet => IHashSet }

object P0044 {
  def main(args: Array[String]) {
    pentagonalgenerator.take(10).foreach(println(_))

    val res = pentagonalgenerator.map {
      case (i, n, seq, set) =>
        println(n)
        seq.dropRight(1)
          .map { o => (o, n - o) }
          .takeWhile { case (a, b) => a < b }
          .filter { case (a, b) => set.contains(b) && set.contains(b - a) }
    }.filterNot(_.isEmpty).next()

    println(res)
    val min @ (a, b) = res.minBy { case (a, b) => b - a }
    println(min)
    println(b - a)
  }

  private def penta(n: Int): Int =
    n * (3 * n - 1) / 2

  private def pentagonalgenerator: Iterator[(Int, Int, ISeq[Int], IHashSet[Int])] = {
    Stream.iterate((1, 1, ISeq(1), IHashSet(1))) {
      case (i, n, seq, set) =>
        val ni = i + 1
        val nn = penta(ni)
        val nseq = seq :+ nn
        val nset = set + nn

        (ni, nn, nseq, nset)
    }.iterator
  }
}
