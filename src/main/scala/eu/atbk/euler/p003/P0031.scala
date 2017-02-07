package eu.atbk.euler.p003

import com.sun.org.apache.bcel.internal.generic.DNEG

object P0031 {

  val denominations = Seq(1, 2, 5, 10, 20, 50, 100, 200).reverse

  def main(args: Array[String]) {

    println(combinations(1)) // 1
    println(combinations(2)) // 2
    println(combinations(5)) // 4
    println(combinations(200)) // 73682

  }

  private def combinations(make: Int): Int = {

    def combinationsImpl(make: Int, prefix: Seq[Int], denom: Seq[Int]): Seq[Seq[Int]] = {
      if (make == 0)
        Seq(prefix)
      else {
        denom match {
          case Nil =>
            Nil
          case (head :: tail) =>
            (0 to (make / head)).flatMap(i => combinationsImpl(make - (i * head), prefix :+ i, tail))
        }
      }

    }

    combinationsImpl(make, Nil, denominations).size
  }
}
