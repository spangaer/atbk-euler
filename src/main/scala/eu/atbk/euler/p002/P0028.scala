package eu.atbk.euler.p002

import scala.collection.immutable.{ Seq => ISeq }

object P0028 {
  def main(args: Array[String]) {
    println(solve(5)) //101
    println(solve(1001)) //669171001
  }

  def solve(dim: Long): Long = {
    Stream.iterate((Seq(1l), 0l)) {
      case (emit, jump) =>
        val newJump = jump + 2
        val newEmit = Stream.iterate(emit.last + newJump, 4)(_ + newJump).to[ISeq]
        (newEmit, newJump)
    }.takeWhile {
      case x @ (emit, jump) =>
        println(x)
        jump < dim
    }.map(_._1).iterator.flatten.sum
  }
}
