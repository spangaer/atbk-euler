package eu.atbk.euler.p000

object P0004 {
  def main(args: Array[String]) {

    println(isPalindrome(9009))
    println(isPalindrome(9019))

    val out = countDown.takeWhile(_ > 99).flatMap(limit => countDown.takeWhile(_ >= limit).map { (limit, _) })
      .map {
        case (a, b) =>
          print((a, b))
          val p = a * b
          println(p)
          p
      }
      .filter(isPalindrome).max

    println(out)
  }

  def isPalindrome(number: Long): Boolean = {
    val digits = number.toString().toSeq

    digits == digits.reverse
  }

  def countDown = Stream.iterate(999l)(_ - 1)
}
