package eu.atbk.euler.p002

import eu.atbk.euler.util.MyMath

object P0026 {
  def main(args: Array[String]) {

    (2 to 20).foreach(i => println(i + "\t" + fraction(1, i)))

    val out = (2 until 1000).map(i => (i, fraction(1, i))).filter(_._2._2.isDefined).maxBy(_._2._2.get.size)

    println(out)
  }

  def fraction(numerator: Int, denominator: Int): (Seq[Int], Option[Seq[Int]]) = {

    def fractionImpl(n: Int, state: (Seq[(Int, Int)])): (Seq[Int], Option[Seq[Int]]) = {
      if (n == 0) {
        (state.map(_._1), None)
      } else {
        state.span { case (digit, previous) => previous != n } match {
          case (_, Nil) =>
            if (n >= denominator) {
              val div = n / denominator
              val rem = n % denominator

              fractionImpl(rem, state :+ (div, n))

            } else {
              fractionImpl(n * 10, state)
            }
          case (prefix, repeat) =>
            (prefix.map(_._1), Some(repeat.map(_._1)))
        }
      }

    }

    fractionImpl(numerator, Seq.empty)
  }
}

