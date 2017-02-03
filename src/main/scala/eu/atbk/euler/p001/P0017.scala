package eu.atbk.euler.p001

import com.ibm.icu.text.NumberFormat
import com.ibm.icu.util.ULocale
import com.ibm.icu.text.RuleBasedNumberFormat
import java.util.Locale
import java.util.regex.Pattern

object P0017 {
  val nf = new RuleBasedNumberFormat(Locale.UK, RuleBasedNumberFormat.SPELLOUT)

  def main(args: Array[String]) {

    (1 to 1000).foreach(x => println(nf.format(x)))

    println(count(342)) //23
    println(count(115)) //20

    println(P0017(5))
    println(P0017(1000))
  }

  def apply(i: Int): Long = {
    val nf = new RuleBasedNumberFormat(Locale.UK, RuleBasedNumberFormat.SPELLOUT)
    val res = (1 to i).map(count(_)).sum

    res
  }

  private val pat = Pattern.compile("[^\\p{Alpha}]")

  def count(i: Long): Long = {
    val add = if (i > 100 && i % 100 != 0) 3l else 0l // add 3 letters for and

    val words = nf.format(i)

    val trim = pat.matcher(words).replaceAll("")

    trim.length() + add
  }
}
