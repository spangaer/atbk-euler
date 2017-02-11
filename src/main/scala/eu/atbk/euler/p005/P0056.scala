package eu.atbk.euler.p005

object P0056 {
  def main(args: Array[String]) {
    val range = (1 until 100)

    val cross = for { i <- range; j <- range } yield (i, j)

    val max = cross.par.map { case (a, b) => BigInt(a).pow(b) }.map(digitalSum).max
    println(max) //
  }

  private def digitalSum(in: BigInt): Long = {
    in.toString().map("" + _).map(java.lang.Long.parseLong).sum
  }
}
