package eu.atbk.euler.p002

import eu.atbk.euler.util.MyMath

object P0025 {
  def main(args: Array[String]) {

    val x = MyMath.fibGenerator
      .dropWhile { case (a, i) => a.toString().length() < 1000 }
      .next._2

    println(x)
  }
}
