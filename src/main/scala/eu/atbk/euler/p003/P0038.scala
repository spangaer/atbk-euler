package eu.atbk.euler.p003

import eu.atbk.euler.util.MyText
import eu.atbk.euler.util.MyMath
import scala.annotation.tailrec
import java.util.concurrent.atomic.AtomicReference

object P0038 {

  // n needs to be bigger 1, so at least two products which concatenated are smaller then 9 digits
  // tha means that anything starting with 5 digites is to large
  val upperBound = 9999
  val upperBoundString = upperBound.toString()

  def main(args: Array[String]) {

    println(MyText.isPanDigtal9((1 to 9).mkString))
    println(upperBoundString)
    println(findPandigital(None, 192))
    println((Seq("9", "19")).sorted.reverse)

    val dataset = (1 to upperBound)

    val out = dataset.foldLeft(None: Option[String])(findPandigital)

    println(out) // 932718654
  }

  private def findPandigital(winner: Option[String], factor: Int): Option[String] = {

    @tailrec
    def findPandigitalImpl(factor: Long, counter: Int, seq: String): Option[String] = {
      val next = factor * counter
      val nextSeq = seq + next

      // only if we have chance that this one is bigger we continue (string compare)
      if ((winner.isEmpty || nextSeq > winner.get)) {
        // only if it's smaller then 9 and all digits are unique it makes sense to continue
        if (nextSeq.length() < 9
          && nextSeq.toSet.size == nextSeq.length()) {
          findPandigitalImpl(factor, counter + 1, nextSeq)
        } else if (nextSeq.length() == 9 && MyText.isPanDigtal9(nextSeq)) {
          Some(nextSeq)
        } else {
          winner
        }
      } else {
        winner
      }

    }

    findPandigitalImpl(factor, 1, "")
  }
}
