package eu.atbk.euler.p003

object P0033 {

  val digits = (1 to 9)

  val fractions = (2 to 9).flatMap(d => (1 until d).map((_, d)))

  def main(args: Array[String]) {

    println(fractions)

    val curious = fractions.flatMap {
      case f @ (n, d) =>
        val o1 = digits.map { x => (f, (10 * x + n, 10 * x + d)) }
        val o2 = digits.map { x => (f, (10 * x + n, 10 * d + x)) }
        val o3 = digits.map { x => (f, (10 * n + x, 10 * d + x)) }
        val o4 = digits.map { x => (f, (10 * n + x, 10 * x + d)) }

        o1 ++ o2 ++ o3 ++ o4
    }.filter {
      case ((n1, d1), (n2, d2)) =>
        n1 * d2 == n2 * d1
    }

    curious.foreach(println(_))

    val prod = curious.foldLeft((1, 1)) { case ((a, b), ((n, d), _)) => (a * n, b * d) }

    println(prod)

    val res = prod match {
      case (n, d) =>
        val gcd = BigInt(n).gcd(d).intValue()
        (n / gcd, d / gcd)
    }

    println(res)
  }

}
