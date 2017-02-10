package eu.atbk.euler.p004

import scala.collection.immutable.{ Seq => ISeq }
import scala.collection.immutable.{ HashSet => IHashSet }
import eu.atbk.euler.util.MyIO
import eu.atbk.euler.util.MyText

object P0042 {
  def main(args: Array[String]) {
    val first10kTrangularNumbers = (1 to 10000).map(i => i * (i + 1) / 2)
    val lookup = first10kTrangularNumbers.to[IHashSet]
    val max = first10kTrangularNumbers.last

    println(max)
    println(lookup.contains(MyText.worth("SKY")))

    val data = MyIO.fileLines("data/p042_words.txt")
      .map(s => s.split(",").map(name => name.substring(1, name.length() - 1)))
      .flatten

    val res = data.filter { w =>
      val worth = MyText.worth(w)
      if (worth > max) {
        throw new IllegalStateException(s"$worth larger then $max")
      }

      lookup.contains(worth)
    }.size

    println(res) // 162

  }
}
