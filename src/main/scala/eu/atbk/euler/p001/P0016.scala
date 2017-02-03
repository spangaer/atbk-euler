package eu.atbk.euler.p001

object P0016 {
  def main(args: Array[String]) {
    println(P0016(15))
    println(P0016(1000))
  }

  def apply(i: Int): Long = {
    val two = BigInt(2)
    val pow = two.pow(i)
    pow.toString.toCharArray().map(d => java.lang.Long.parseLong("" + d)).sum
  }
}
