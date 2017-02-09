package eu.atbk.euler.p003

import eu.atbk.euler.util.MyMath

object P0034 {
  def main(args: Array[String]) {

    println(decimalFac(145)) // 145

    val out = boundedStream.filter(x => x == decimalFac(x)).sum
    println(out)
  }

  val facs = (0 to 9).map(MyMath.fac(_))

  private def decimalFac(in: Long): Long = {
    in.toString().toCharArray()
      .map(x => Integer.parseInt("" + x))
      .map(facs).sum
  }

  private def boundedStream: Iterator[Long] =
    Stream.iterate(11l)(_ + 1)
      // once number exceeds (decimals + 1) * fac(*), sum can never reach number anymore
      .takeWhile(x => x <= (x.toString().length() + 1) * facs(9))
      .iterator

}
