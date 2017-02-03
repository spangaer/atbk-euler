package eu.atbk.euler.p001

/**
 * Conceptually the solution is the same as for P0018, but in this case we don't need to keep the subtrees in cache.
 * We just determine the max path for each subtree level by level, working bottom up
 *
 * @author jean
 *
 */
object P0018b {
  def main(args: Array[String]) {

    println(solve(P0018.input1Raw)) //23

    println(solve(P0018.input2Raw)) // 1074

    println(solve(P0018.input3Raw)) // 7273

  }

  def solve(input: Seq[Seq[Long]]): Long = {

    val reverse = input.reverse
    val start = reverse.head
    val solver = reverse.tail

    solver.foldLeft(start) {
      case (toMax, toSum) =>
        val maxes = (0 until (toMax.size - 1)).map(i => Math.max(toMax(i), toMax(i + 1)))

        toSum.zip(maxes).map { case (a, b) => a + b }
    }.head

  }
}
