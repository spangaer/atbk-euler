package eu.atbk.euler.p002

object P0029 {
  def main(args: Array[String]) {
    println(solve(5)) //15
    println(solve(100)) //9183
  }

  def solve(n: Int): Int = {
    val range = (2 to n)

    range.flatMap(a => range.map(b => (a, b))).map { case (a, b) => BigInt(a).pow(b) }.sorted.distinct.size
  }
}
