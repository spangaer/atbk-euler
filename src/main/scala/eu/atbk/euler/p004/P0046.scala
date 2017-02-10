package eu.atbk.euler.p004

import eu.atbk.euler.util.MyMath

object P0046 {
  def main(args: Array[String]) {

    oddCompositeGenerator.take(6).map(doIt).foreach(println(_))

    val res = oddCompositeGenerator.map(doIt).map { x =>
      println(x)
      x
    }.filter(_._2.isEmpty).next()

    println(res)

  }

  def oddCompositeGenerator: Iterator[Long] = {
    // googeloo: an odd composite is a odd number that is not a prime
    Stream.iterate(9l)(_ + 2).iterator
      .filterNot(MyMath.isPrimeBelow10to12th)
  }

  def doIt(composite: Long): (Long, Option[(Long, Long)]) = {
    val lowerPrimes = MyMath.primesBelow1million.takeWhile(_ < composite)

    lowerPrimes.map { p =>

      MyMath.iterator(1).map(x => (x, 2 * x * x)).takeWhile(_._2 + p <= composite)
        .find(_._2 + p == composite)
        .map { case (a, _) => (p, a) }
    }
      .find(_.isDefined)
      .flatten match {
        case o: Some[_] =>
          (composite, o)
        case None =>
          (composite, None)
      }
  }
}
