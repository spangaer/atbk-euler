package eu.atbk.euler.p004

import eu.atbk.euler.util.MyMath
import scala.collection.immutable.{ Seq => ISeq }

object P0040 {
  def main(args: Array[String]) {

    println(irrationalIter.takeWhile(_._2 < 12).map(x => selectDigit(Seq(12), x)).flatten.to[ISeq])

    val selectors = (0 to 6).map(MyMath.power(10, _).toInt)
    println(selectors)
    val selection = irrationalIter.takeWhile(_._2 < selectors.last).map(x => selectDigit(selectors, x)).flatten.to[ISeq]
    println(selection)
    println(selection.product) // 210

  }

  private def irrationalIter: Iterator[(Int, Int, Int)] =
    Stream.iterate((1, 0, 1)) {
      case (i, bottom, top) =>
        val next = i + 1
        val delta = next.toString().length()

        (next, top, top + delta)
    }.iterator

  private def selectDigit(selectors: Seq[Int], element: (Int, Int, Int)): Seq[Int] = {
    val (index, bottom, top) = element
    val indexS = index.toString()
    selectors.filter(s => s > bottom && s <= top).map { s =>
      val i = s - bottom - 1
      Integer.parseInt("" + indexS(i))
    }
  }
}
