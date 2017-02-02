package eu.atbk.euler.p001

import scala.collection.immutable.BitSet

object P0010b {

  // NOT FASTER

  def main(args: Array[String]) {

    primeGenerator(100).foreach(println(_))

    val limit = 2 * 1000 * 1000 + 1

    val out = primeGenerator(limit)
      .map { x =>
        println(x)
        x
      }
      .sum

    println(out)

  }

  def primeGenerator(limit: Int): Iterator[Int] = {
    val start = BitSet.empty + 2 + 3

    Stream.iterate((Seq(2, 3), start, false)) {
      case (_, _, true) =>
        (Seq.empty, BitSet.empty, true)
      case (current, zif, stop) =>
        val lastPrime = current.last
        val sq = lastPrime * lastPrime
        val finalLimit = Math.min(limit, sq)
        val newWorkZif = zif ++ (lastPrime to finalLimit)

        val newZif = zif.takeWhile(_ <= lastPrime)
          .foldLeft(newWorkZif) {
            case (a, b) =>
              a.filter { toPrime =>
                (toPrime <= lastPrime) || (toPrime % b != 0)
              }
          }

        val newPrimes = newZif.dropWhile(_ <= lastPrime).takeWhile(_ <= finalLimit).toSeq

        (newPrimes, newZif, finalLimit == limit)
    }
      .takeWhile { case (primes, _, stop) => !(primes.isEmpty && stop) }
      .map(_._1)
      .flatMap(x => x)
      .iterator
  }
}
