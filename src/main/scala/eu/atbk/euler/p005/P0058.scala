package eu.atbk.euler.p005

import scala.collection.immutable.{ Seq => ISeq }
import scala.collection.immutable.{ HashSet => IHashSet }
import eu.atbk.euler.util.MyMath

object P0058 {
  def main(args: Array[String]) {
    println(solve(63, 5))
    println(solve(10, 1)) //26241
  }

  //  building on P28
  def solve(prct: Int, minDim: Int): Long = {
    Stream.iterate((ISeq(1l), 0l, 0l, 1l)) {
      case (emit, jump, primes, total) =>
        val newJump = jump + 2
        val newEmit = Stream.iterate(emit.last + newJump, 4)(_ + newJump).to[ISeq]
        (newEmit, newJump, primes + newEmit.count(MyMath.isPrimeBelow10to12th), total + 4)
    }.dropWhile {
      case x @ (emit, jump, primes, total) =>
        println(x)
        val now = primes * 100 / total
        println(s"$now%")
        now >= prct || (jump + 1) <= minDim
    }.head._2 + 1 //dim is jump + 1
  }
}
