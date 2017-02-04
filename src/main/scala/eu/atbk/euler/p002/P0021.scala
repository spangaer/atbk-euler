package eu.atbk.euler.p002

import eu.atbk.euler.util.MyMath
import scala.collection.immutable.{ Set => ISet }
import scala.annotation.tailrec

object P0021 {
  def main(args: Array[String]) {

    println(amicable(220)) // Some(284)

    println(findAll(10000).sum) //31626

  }

  def findAll(limit: Long): Set[Long] = {

    @tailrec
    def findAllImpl(counter: Long, collector: ISet[Long]): Set[Long] = {
      if (counter >= limit) {
        collector
      } else if (collector.contains(counter)) {
        findAllImpl(counter + 1, collector)
      } else {
        val newCollector = amicable(counter) match {
          case Some(other) =>
            collector + counter + other
          case None =>
            collector
        }

        findAllImpl(counter + 1, newCollector)
      }
    }

    findAllImpl(1, ISet.empty)
  }

  def amicable(in: Long): Option[Long] = {
    val other = MyMath.devidors(in).sum
    val check = MyMath.devidors(other).sum

    if (in != other && check == in) {
      Some(other)
    } else {
      None
    }
  }

}
