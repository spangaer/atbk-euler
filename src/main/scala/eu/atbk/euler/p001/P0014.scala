package eu.atbk.euler.p001

object P0014 {

  private val collatz: (Long => Long) = {
    case l if (l % 2 == 0) =>
      l / 2
    case l if (l == 1) =>
      // end condition cheat
      0
    case l =>
      3 * l + 1
  }

  private def collatzSeq(start: Long): Iterator[Long] =
    Stream.iterate(start)(collatz).takeWhile(_ > 0).iterator

  def main(args: Array[String]) {
    println(collatzSeq(13).size)

    val out = (1 until (1000 * 1000)).iterator
      .map { i => (i, collatzSeq(i).size) }
      .map { i =>
        println(i)
        i
      }
      .maxBy(_._2)

    println(out)
  }
}
