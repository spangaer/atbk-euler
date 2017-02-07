package eu.atbk.euler.p002

import eu.atbk.euler.util.MyMath

object P0023 {
  val isAbundant = (0 to 28123).map(abundant).toSeq

  def main(args: Array[String]) {

    println(abundant((12))) //true 
    println(isAbundant(13)) // false
    println(isAbundant(18)) // true

    println(hasAbundantSum(24)) // true
    println(hasAbundantSum(25)) // false

    println((0 to 28123).filterNot(hasAbundantSum).sum) // 4179871
  }

  def abundant(in: Int): Boolean = {
    if (in < 12)
      false
    else
      MyMath.devidors(in).sum > in
  }

  def hasAbundantSum(in: Int): Boolean = {
    if (in < 24)
      false
    else {
      (12 to (in / 2)).iterator.filter { i =>
        isAbundant(i) && isAbundant(in - i)
      }.hasNext
    }
  }
}
