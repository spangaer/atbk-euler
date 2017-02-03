package eu.atbk.euler.p001

import java.util.Calendar
import java.util.TimeZone
import java.text.DateFormat

object P0019 {
  def main(args: Array[String]) {

    println(firstDowGenerator(1901, 2001).filter(_ == Calendar.SUNDAY).size)

  }

  val utc = TimeZone.getTimeZone("UTC")

  def firstDowGenerator(startYear: Int, endYearExlusive: Int): Iterator[Int] = {

    val cal = Calendar.getInstance(utc)
    val format = DateFormat.getDateInstance(DateFormat.FULL)
    format.setTimeZone(utc)

    for (i <- Seq(Calendar.HOUR_OF_DAY, Calendar.MINUTE, Calendar.SECOND, Calendar.MILLISECOND))
      cal.set(i, 5)

    cal.set(Calendar.YEAR, startYear)
    cal.set(Calendar.DAY_OF_YEAR, 1)

    Stream.iterate((cal.get(Calendar.DAY_OF_WEEK), cal)) {
      case (dow, cal) =>
        cal.add(Calendar.MONTH, 1)
        println(format.format(cal.getTime))
        (cal.get(Calendar.DAY_OF_WEEK), cal)
    }
      .takeWhile {
        _._2.get(Calendar.YEAR) < endYearExlusive
      }
      .map {
        _._1
      }.iterator
  }

}
