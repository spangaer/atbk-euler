package eu.atbk.euler.p003

import scala.collection.mutable.Buffer
import scala.collection.immutable.{ Seq => ISeq }

object P0039 {
  def main(args: Array[String]) {

    println(findSols(120))

    val sol = (12 to 1000).map { i =>
      val out = findSols(i)
      println(s"$i\t$out")
      out
    }.maxBy(_.size)

    println(sol)
    println(sol.size)
    val (a, b, c) = sol(0)
    println(a + b + c) // 840

  }

  def findSols(i: Int): Seq[(Int, Int, Int)] = {
    val buffer = Buffer.empty[(Int, Int, Int)]

    val aRange = i / 3

    for (a <- (1 to aRange)) {
      val bRange = (i - a) / 2
      for (b <- (a to bRange)) {
        val c = (i - a - b)

        if (a * a + b * b == c * c) {
          buffer += ((a, b, c))
        }
      }
    }

    buffer.to[ISeq]
  }

}
