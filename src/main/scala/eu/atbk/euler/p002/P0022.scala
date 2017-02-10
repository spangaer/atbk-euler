package eu.atbk.euler.p002

import eu.atbk.euler.util.MyIO
import eu.atbk.euler.util.MyText

object P0022 {

  def main(args: Array[String]) {

    println(MyText.worth("COLIN")) //53

    val names = MyIO.fileLines("data/p022_names.txt")
      .map(s => s.split(",").map(name => name.substring(1, name.length() - 1)))
      .flatten.sorted

    println(names(938 - 1)) //COLIN  

    val out = names.zipWithIndex.map {
      case (name, i_m) =>
        val i = i_m + 1
        MyText.worth(name) * i
    }.sum

    println(out) // 871198282
  }

}
