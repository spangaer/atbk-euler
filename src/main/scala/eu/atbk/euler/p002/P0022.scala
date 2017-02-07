package eu.atbk.euler.p002

import eu.atbk.euler.util.MyIO

object P0022 {

  val letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toCharArray().zipWithIndex.map { case (l, i) => (l, i + 1) }.toMap

  def main(args: Array[String]) {

    println(worth("COLIN")) //53

    val names = MyIO.fileLines("data/p022_names.txt")
      .map(s => s.split(",").map(name => name.substring(1, name.length() - 1)))
      .flatten.sorted

    println(names(938 - 1)) //COLIN  

    val out = names.zipWithIndex.map {
      case (name, i_m) =>
        val i = i_m + 1
        worth(name) * i
    }.sum

    println(out)
  }

  def worth(in: String): Long = {
    in.toCharArray().map(letters).sum
  }

}
