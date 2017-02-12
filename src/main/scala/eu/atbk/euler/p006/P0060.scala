package eu.atbk.euler.p006

import scala.collection.immutable.{ Seq => ISeq }
import scala.collection.immutable.{ HashSet => IHashSet }
import scala.collection.immutable.{ HashMap => IHashMap }
import scala.collection.parallel.immutable.{ ParSeq => IParSeq }
import eu.atbk.euler.util.MyMath
import scala.annotation.tailrec

object P0060 {
  def main(args: Array[String]) {
    println(solve2(2)) // List(3, 7)
    println(solve2(3)) // List(3, 37, 67)
    println(solve2(4)) // List(3, 7, 109, 673)
    val res = solve2(5)
    println(res) // List(13, 5197, 5701, 6733, 8389)
    println(res.sum) // 26033
  }

  private def allPrimeConcats(in: Seq[Long]): Boolean = {
    in.combinations(2).forall { two =>
      val p1 = java.lang.Long.parseLong(two(0).toString() + two(1).toString())
      val p2 = java.lang.Long.parseLong(two(1).toString() + two(0).toString())

      MyMath.isPrimeBelow10to12th(p1) && MyMath.isPrimeBelow10to12th(p2)
    }
  }

  //  create a map of primes to primes with which they can be combined

  private lazy val basePairOptions = {
    val startSet = MyMath.primesBelow1million.takeWhile(_ < 10000) // "cheat" trim

    startSet.to[IParSeq].map { p =>
      val pairable = startSet.filter(o => o != p && allPrimeConcats(Seq(p, o)))
      (p, pairable)
    }.filterNot(_._2.isEmpty)
  }

  private lazy val basePairLookup = basePairOptions.seq.toMap

  private def solve2(n: Int): Seq[Long] = {

    @tailrec
    def solve2Impl(data: IParSeq[Seq[Long]], length: Int): Seq[Long] = {
      if (length == n) {
        val sorted = data.seq.sortBy(_.sum)
        sorted.head
      } else {
        val newData = data
          .map { seqSoFar =>
            // find all options and intersect them with each other
            val newOptions = seqSoFar.map(basePairLookup)
            val newOptionsInterSect = newOptions.tail.foldLeft(newOptions.head) { case (i, o) => i.intersect(o) }
            (seqSoFar, newOptionsInterSect)
          }
          .filterNot(_._2.isEmpty) // all new sequences to be considered
          .flatMap { case (seqSoFar, options) => options.map(o => (seqSoFar :+ o).sorted) }.distinct

        // next iteration
        solve2Impl(newData, length + 1)
      }
    }

    val workStart = basePairOptions.flatMap { case (p, list) => list.map(o => ISeq(p, o).sorted) }.distinct
    solve2Impl(workStart, 2)
  }

  /**
   * TOO SLOW
   * @param n
   * @return
   */
  private def solve(n: Int): Seq[Seq[Long]] = {

    val iterator = MyMath.primesBelow1million.iterator

    iterator.map(findCombos(n, _)).dropWhile(_.isEmpty).next()
  }

  /**
   * TOO SLOW
   * @param n
   * @param prime
   * @return
   */
  private def findCombos(n: Int, prime: Long): Seq[Seq[Long]] = {
    val toCombine = MyMath.primesBelow1million.takeWhile(_ < prime)

    try {
      toCombine.combinations(n - 1).to[ISeq].par.map(_ :+ prime).filter(allPrimeConcats).to[ISeq]
    } finally {
      println(prime)
    }

  }
}
