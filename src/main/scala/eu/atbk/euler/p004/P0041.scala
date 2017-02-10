package eu.atbk.euler.p004

import eu.atbk.euler.util.MyMath
import scala.collection.GenSeq

object P0041 {
  def main(args: Array[String]) {

    val start = System.currentTimeMillis()

    val out = panDIter.foldLeft(1l) {
      case (max, seq) =>
        seq.aggregate(max)({
          case (max, next) =>
            if (next > max && MyMath.isPrimeBelow10to12th(next)) {
              println(next)
              next
            } else
              max
        }, {
          case (a, b) =>
            Math.max(a, b)
        })
    }

    val end = System.currentTimeMillis()
    println(out) // 7652413

    println(s"in: ${end - start} ms") // 5 s

  }

  def panDIter: Iterator[GenSeq[Long]] = {
    (9 to 1 by -1) // reverse to hit the highest first (given the max check it quickly limits the search space)
      .iterator
      .map { i =>
        (i to 1 by -1).permutations.map(MyMath.digitSeqToNumber).toSeq.par
      }
  }

}
