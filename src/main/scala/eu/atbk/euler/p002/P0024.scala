package eu.atbk.euler.p002

import scala.collection.immutable.{ Seq => ISeq }

object P0024 {

  def main(args: Array[String]) {
    println((0 to 2).permutations.to[ISeq])

    val out = (0 to 9).permutations.drop((1000 * 1000 - 1)).next().mkString
    println(out)
  }

}
