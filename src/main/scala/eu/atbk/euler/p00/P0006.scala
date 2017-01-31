package eu.atbk.euler.p00

object P0006 {
  def main(args: Array[String]) {

    println(P0006(10))

    println(P0006(100))
  }

  def apply(i: BigInt): BigInt = {
    val sumOfSqares = bigIntStream.takeWhile(_ <= i).map(x => x * x).sum
    val sum = bigIntStream.takeWhile(_ <= i).sum
    val squareOfSum = sum * sum
    squareOfSum - sumOfSqares
  }

  def bigIntStream: Iterator[BigInt] =
    Stream.iterate(BigInt(1))(_ + 1).iterator
}
