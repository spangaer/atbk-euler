package eu.atbk.euler.p001

import java.nio.file.Files
import java.nio.file.Paths
import java.nio.charset.StandardCharsets
import scala.collection.JavaConverters._
import scala.collection.mutable.{ HashMap => MutHashMap }
import eu.atbk.euler.util.MyIO

/**
 * Also solves p67
 *
 * @author jean
 *
 */
object P0018 {
  def main(args: Array[String]) {

    println(solve(input1)) //23

    println(solve(input2)) // 1074

    println(solve(input3)) // 7273

  }

  def solve(input: Entry): Long = {

    def solveDo(top: Entry, accumulator: MutHashMap[Entry, Long]): Long = {
      top match {
        case key @ Entry(value, Some((left, rigth))) =>
          // cache the max path of every sub-tree
          accumulator.getOrElseUpdate(top, // using pure object equals and hashcode
            Math.max(solveDo(left, accumulator), solveDo(rigth, accumulator)) + value)
        case Entry(value, None) =>
          value
      }
    }

    solveDo(input, MutHashMap.empty)

  }

  private def parseInput(numberRows: Seq[Seq[Long]]): Entry = {

    val bottomUp = numberRows.reverse
    val bottom = bottomUp.head.map(Entry(_, None))
    val top = bottomUp.tail.foldLeft(bottom) {
      case (lower, upper) =>
        upper.zipWithIndex.map { case (value, index) => Entry(value, Some((lower(index), lower(index + 1)))) }
    }.head
    top
  }

  val input1Raw = MyIO.string2NumberSeqSeq("""
3
7 4
2 4 6
8 5 9 3
""")

  private val input1 = parseInput(input1Raw)

  val input2Raw = MyIO.string2NumberSeqSeq("""
75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
""")

  private val input2 = parseInput(input2Raw)

  def input3Raw = MyIO.file2NumberSeqSeq("data/p067_triangle.txt")

  private def input3 = parseInput(input3Raw)

  private case class Entry(value: Long, children: Option[(Entry, Entry)]) {

    // use actualy ojbect equalty, each node in tree should only have one instance
    // this way an entry is a good cache key for caching the max path underneath it

    override def hashCode = System.identityHashCode(this)
    override def equals(a: Any) = super.equals(a)

  }
}

