package eu.atbk.euler.util

import scala.math.ScalaNumber
import scala.math.ScalaNumericConversions
import scala.math.BigInt
import scala.math.BigInt

class Fraction private[Fraction] (val numerator: BigInt, val denominator: BigInt) extends ScalaNumber with ScalaNumericConversions {

  // Members declared in scala.math.ScalaNumericAnyConversions
  def doubleValue: Double = numerator.doubleValue() / numerator.longValue()
  def floatValue: Float = doubleValue().toFloat
  def intValue: Int = underlying.intValue()
  def isWhole: Boolean = denominator == 1
  def longValue: Long = underlying.longValue()

  // Members declared in scala.math.ScalaNumericConversions
  override lazy val underlying = numerator / denominator

  def equalDenominators(that: Fraction): (Fraction, Fraction) = {
    val newDenom = denominator * that.denominator
    (Fraction(numerator * that.denominator, newDenom), Fraction(that.numerator * denominator, newDenom))
  }

  //  def equals(that: BigInt): Boolean = equals(new Fraction(that, 1))

  def equals(that: Fraction): Boolean = {
    if (denominator == that.denominator) {
      numerator == that.numerator
    } else {
      equalDenominators(that) match {
        case (me, other) => me equals other
      }
    }
  }

  def compare(that: Fraction): Int = {
    if (denominator == that.denominator) {
      numerator compare that.numerator
    } else {
      equalDenominators(that) match {
        case (me, other) => me compare other
      }
    }
  }

  def <=(that: Fraction): Boolean = compare(that) <= 0

  def >=(that: Fraction): Boolean = compare(that) >= 0

  def <(that: Fraction): Boolean = compare(that) < 0

  def >(that: Fraction): Boolean = compare(that) > 0

  def +(that: Fraction): Fraction = {
    if (denominator == that.denominator) {
      Fraction(numerator + that.numerator, denominator)
    } else {
      equalDenominators(that) match {
        case (me, other) => me + other
      }
    }
  }

  def -(that: Fraction): Fraction = this + (-that)

  def *(that: Fraction): Fraction = Fraction(numerator * that.numerator, denominator * that.denominator)

  def /(that: Fraction): Fraction = this * Fraction(that.denominator, that.numerator)

  def min(that: Fraction): Fraction =
    if (this <= that) {
      this
    } else
      that

  def max(that: Fraction): Fraction =
    if (this >= that) {
      this
    } else
      that

  def pow(exp: Int) = Fraction(numerator.pow(exp), denominator.pow(exp))

  def unary_- = Fraction(-numerator, denominator)

  def abs = Fraction(numerator.abs, denominator)

  /**
   * Returns the sign of this BigInt;
   *   -1 if it is less than 0,
   *   +1 if it is greater than 0,
   *   0  if it is equal to 0.
   */
  def signum: Int = numerator.signum

  def charValue = intValue.toChar

  override def toString(): String = s"$numerator/$denominator"

  def simplify: Fraction = {
    val gcd = numerator gcd denominator
    Fraction(numerator / gcd, denominator / gcd)
  }

}

object Fraction {

  /**
   * ensure the sign is always in the numerator
   * and is smallest representation
   *
   * @param numerator
   * @param denominator
   * @return
   */
  def apply(numerator: BigInt, denominator: BigInt): Fraction = new Fraction(denominator.signum * numerator, denominator.abs)

  object Implicits {
    import scala.language.implicitConversions

    implicit def intToFraction(int: Int): Fraction = Fraction(int, 1)
    implicit def longToFraction(long: Long): Fraction = Fraction(long, 1)
    implicit def bigIntToFraction(bigInt: BigInt): Fraction = Fraction(bigInt, 1)
  }

}
