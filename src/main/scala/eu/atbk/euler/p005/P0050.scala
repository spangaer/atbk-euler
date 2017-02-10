package eu.atbk.euler.p005

import eu.atbk.euler.util.MyMath
import scala.collection.immutable.{ Seq => ISeq }
import scala.collection.immutable.{ HashSet => IHashSet }

object P0050 {
  def main(args: Array[String]) {

    println(solve(100)) // 41,6 
    println(solve(1000)) // 953,21
    println(solve(1000 * 1000)) // 997651,543

  }

  private def solve(n: Int): (Long, Int, Seq[Long]) = {
    val dataset = MyMath.primesBelow1million.takeWhile(_ < n)
    val max = dataset.last
    val lookup = dataset.to[IHashSet]

    // looks complex, but is all about reducing search space as fast as possible

    val sumSeq = (0 until dataset.size).par.aggregate(Seq(max))({
      case (currentWinner, i) =>
        // make subset starting at index
        val mydata = dataset.drop(i)
        if (mydata.size <= currentWinner.size) {
          // can never be bigger than current winner
          currentWinner
        } else {
          // don't care about anything short than current winner
          val startTakeCount = currentWinner.size + 1
          val startTake = mydata.take(startTakeCount)
          val startSum = startTake.sum
          if (startSum > max) {
            currentWinner
          } else {
            // when here we still have a chance to beat the current winner
            // make sequence longer, step by step
            val out = Stream.iterate((startTakeCount, startTake, startSum)) {
              case (takeCount, take, sum) =>
                (takeCount + 1, take :+ mydata(takeCount), sum + mydata(takeCount))
            }.takeWhile {
              case (takeCount, take, sum) =>
                // until it exceeds max prime or end of mydata
                takeCount < mydata.size && sum <= max
            }.filter {
              // keep only prime sums
              case (takeCount, take, sum) =>
                lookup.contains(sum)
            }.map(_._2).lastOption.getOrElse {
              // last one is the longest, if none, return current winner
              currentWinner
            }

            out
          }
        }
    }, { // always select longest
      case (a, b) =>
        if (a.size >= b.size)
          a
        else
          b
    })

    (sumSeq.sum, sumSeq.size, sumSeq)

    // simple implementatio too slow
    //    val sumSeq = (dataset.size to 2 by -1).iterator.flatMap { i =>
    //      dataset.sliding(i).filter { subseq =>
    //        val sum = subseq.fold(0l) {
    //          case (sum, p) =>
    //            if (sum <= max)
    //              sum + p
    //            else
    //              sum
    //        }
    //        sum <= max && lookup.contains(sum)
    //      }
    //    }.next()
  }
}
