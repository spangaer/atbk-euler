package eu.atbk.euler.p001

import eu.atbk.euler.util.MyMath
import scala.concurrent.forkjoin.ForkJoinPool

object P0010 {
  // 142913828922

  def main(args: Array[String]) {
    println(ForkJoinPool.getCommonPoolParallelism)

    val limit = 2l * 1000 * 1000

    val out = MyMath.primeGenerator.takeWhile(_ < limit)
      .map { x =>
        println(x)
        x
      }
      .sum

    println(out)
  }
}
