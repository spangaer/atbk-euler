package eu.atbk.euler.p003

import scala.collection.immutable.{ Seq => ISeq }
import eu.atbk.euler.util.MyMath

object P0032 {

  val digits = (1 to 9)

  def main(args: Array[String]) {

    val out = (1 to digits.size - 2).iterator
      .flatMap { i =>
        val combos = digits.combinations(i)

        combos.map(combo => (combo, digits.diff(combo)))
      }
      .flatMap {
        case (combo, remain) =>
          (1 to remain.size - 1).iterator
            .flatMap { i =>
              val combos2 = remain.combinations(i)

              combos2.map(combo2 => (combo, combo2, remain.diff(combo2).sorted.to[ISeq]))
            }

      }.flatMap {
        case (a, b, c) =>
          a.permutations.flatMap { a =>
            b.permutations.map { b =>
              (a, b, c)
            }
          }
      }
      .map {
        case (a, b, c) =>
          (a, b, MyMath.digitSeqToNumber(a) * MyMath.digitSeqToNumber(b), c)
      }.filter {
        case (a, b, n, c) =>
          val test = n.toString().toCharArray().map { x => Integer.parseInt("" + x) }.sorted.to[ISeq]
          test == c
      }.map { x =>
        println(x)
        x
      }
      .map(_._3).toSeq
      .distinct
      .sum

    println(out) //45228
  }

}
