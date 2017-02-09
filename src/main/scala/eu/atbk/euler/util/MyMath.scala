package eu.atbk.euler.util

import scala.annotation.tailrec
import scala.collection.GenSeq
import scala.collection.parallel.immutable.ParSeq
import scala.collection.immutable.{ Seq => ISeq }
import scala.collection.JavaConverters._
import java.nio.file.Paths
import java.nio.file.Files

object MyMath {

  def primeGenerator: Iterator[Long] = {
    Stream
      .iterate((2l, Seq(2l): GenSeq[Long], false)) {
        case (now, primes, stop) =>
          val next = now + 1
          if (primes.takeWhile { p =>
            val sq = p * p
            sq > 0 && sq <= next
          }.exists(next % _ == 0)) {
            // not a prime
            (next, primes, stop)
          } else {
            // prime
            (next, primes :+ next, next * next < 0)
          }
      }
      .takeWhile {
        case (_, _, stop) =>
          !stop
      }
      .filter {
        case (a, b, _) =>
          // only keep primes in sequence
          b.last == a
      }
      .map(_._1) // don't need to drag along state
      .iterator
  }

  lazy val primesBelow1million: Seq[Long] = {
    val path = Paths.get("data/primes_to_1million.txt")

    val fromFile: Option[Seq[Long]] = if (Files.exists(path)) {
      try {
        val temp = Files.readAllLines(path).asScala.filterNot(_.isEmpty()).map(x => java.lang.Long.parseLong(x)).to[ISeq]
        val sum = temp.foldLeft(0l)(checkSum)

        if (sum == 7308170383993554604l) {
          Some(temp)
        } else {
          None
        }

      } catch {
        case x: Exception =>
          x.printStackTrace()
          None
      }
    } else
      None

    fromFile match {
      case Some(done) =>
        done
      case None =>
        val limit = 1000 * 1000
        val out = MyMath.primeGenerator.takeWhile(_ <= limit).map { p =>
          println(p)
          p
        }.to[ISeq]

        val sum = out.foldLeft(0l)(checkSum)

        println(s"checksum ${sum}")

        val stream = Files.newBufferedWriter(path)

        try {
          out.foldLeft(stream) {
            case (stream, prime) =>
              stream.write(prime.toString())
              stream.newLine()
              stream
          }
        } finally {
          stream.close()
        }

        out
    }
  }

  private def checkSum(sum: Long, add: Long): Long = {
    rotate(sum, 15) ^ add
  }

  private def rotate(in: Long, bits: Long): Long = {
    val left = Math.abs(bits) % 64
    val right = 64 - left
    val out = (in << left) | (in >>> right)
    out
  }

  private def sqrt(number: BigInt): BigInt = {
    def next(n: BigInt, i: BigInt): BigInt = (n + i / n) >> 1
    val one = BigInt(1)
    val n = one
    val n1 = next(n, number)
    def sqrtHelper(n: BigInt, n1: BigInt): BigInt = if ((n1 - n).abs <= one) List(n1, n).max else sqrtHelper(n1, next(n1, number))
    sqrtHelper(n, n1)
  }

  def sqrt(number: Long): Long =
    sqrt(BigInt(number)).longValue()

  def primeDecompose(number: Long): Seq[Long] =
    primeDecompose(number, sqrt(number) + 1)

  @tailrec
  private def primeDecompose(number: Long, limit: Long, current: Seq[Long] = Seq.empty, start: Long = 2, primes: Iterator[Long] = primeGenerator): Seq[Long] = {

    if (limit < start) { // the rest is prime
      if (number != 1)
        current :+ number
      else
        current
    } else {
      val div = number / start
      val rem = number - (start * div)
      if (rem == 0) {
        // 
        val limit = sqrt(div) + 1
        primeDecompose(div, limit, current :+ start, start, primes)
      } else {
        primeDecompose(number, limit, current, primes.next(), primes)
      }
    }
  }

  def isPrime(in: Long) =
    in > 1 && primeDecompose(in).last == in

  val ZERO = BigInt(0)
  val ONE = BigInt(1)

  def fac(in: BigInt): BigInt = {
    @tailrec
    def facImpl(current: BigInt, prod: BigInt): BigInt = {
      current match {
        case ZERO =>
          1
        case ONE =>
          prod
        case other =>
          facImpl(other - 1, prod * other)
      }
    }

    facImpl(in, ONE)
  }

  def fac(in: Long): Long = {
    @tailrec
    def facImpl(current: Long, prod: Long): Long = {
      current match {
        case 0 =>
          1
        case 1 =>
          prod
        case other =>
          facImpl(other - 1, prod * other)
      }
    }

    facImpl(in, 1)
  }

  def devidors(number: Long): Seq[Long] = {
    val primedecomp = MyMath.primeDecompose(number)
    val out = 1l +: ((1 until primedecomp.size).flatMap(i => primedecomp.combinations(i)).distinct.map(_.product))
    out
  }

  def fibGenerator: Iterator[(BigInt, Long)] = {
    Stream.iterate((BigInt(1), BigInt(1), 1l)) {
      case (a, b, i) => (b, a + b, i + 1)
    }
      .map {
        case (a, b, i) => (a, i)
      }.iterator
  }

  def power(x: Long, p: Int): Long = {
    Stream.fill(p)(x).product
  }

  def digitSeqToNumber(in: Seq[Int]): Long =
    in.foldLeft(0l) { case (current, next) => current * 10 + next }

  def numberToDigitSeq(in: Long): Seq[Int] =
    in.toString().toCharArray().map(x => Integer.parseInt("" + x))

  def rotations[T](seq: Seq[T]): Seq[Seq[T]] = {
    val out = (0 until seq.size).map { i =>
      val (a, b) = seq.splitAt(i)
      b ++ a
    }

    out
  }
}
