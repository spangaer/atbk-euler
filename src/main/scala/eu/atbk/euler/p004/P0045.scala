package eu.atbk.euler.p004

import scala.collection.immutable.{ Seq => ISeq }
import scala.collection.immutable.{ HashSet => IHashSet }

object P0045 {
  def main(args: Array[String]) {

    val res = generator.filter {
      case (i, (tseq, _), (_, pset), (_, hset)) =>
        println(s"$i\t${tseq.last}")
        pset.contains(tseq.last) && hset.contains(tseq.last)
    }.map(_._2._1.last).take(3).to[ISeq]

    println(res)
  }

  private def trian(n: Long): Long =
    n * (n + 1) / 2

  private def penta(n: Long): Long =
    n * (3 * n - 1) / 2

  private def hexa(n: Long): Long =
    n * (2 * n - 1)

  type Data = (ISeq[Long], IHashSet[Long])

  private def doIt(n: Long, f: (Long => Long), data: Data): Data = {
    val (seq, set) = data
    val next = f(n)
    (seq :+ next, set + next)
  }

  private def generator: Iterator[(Long, Data, Data, Data)] = {
    val startData @ (startSet, startMap) = (ISeq(1l), IHashSet(1l))

    Stream.iterate((1l, startData, startData, startData)) {
      case (i, tri, pen, hex) =>
        val ni = i + 1

        (ni, doIt(ni, trian, tri), doIt(ni, penta, pen), doIt(ni, hexa, hex))
    }.iterator
  }
}
