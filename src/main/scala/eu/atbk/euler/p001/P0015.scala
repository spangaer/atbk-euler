package eu.atbk.euler.p001

import scala.collection.mutable.{ Map => MutMap }

object P0015 {

  /**
   * doesn't work fast enough (need to be smarter about this)
   *
   * @param row
   * @param col
   * @return
   */
  def options(row: Long, col: Long): Long =
    (row, col) match {
      case (1, col) =>
        col + 1 // number of cells + 1 // will cover case 1,1 => 2
      case (row, 1) =>
        row + 1 // number of cells + 1
      case (row, col) =>
        // number of routes is the sum of the number of routes of this binary choice
        options(row - 1, col) + options(row, col - 1)
    }

  def options2(row: Long, col: Long): Long = {
    // hide need for accumulator to avoid accidents

    def options2Do(row: Long, col: Long, accumulator: MutMap[(Long, Long), Long]): Long = {
      if (row <= col) {

        (row, col) match {
          case (1, col) =>
            col + 1 // number of cells + 1 // will cover case 1,1 => 2
          // given that row is always the smallest, we don't need to check that
          case key @ (row, col) =>
            // cache 'expensive' results for reuse            
            accumulator.getOrElseUpdate(key, {
              // number of routes is the sum of the number of routes of this binary choice
              options2Do(row - 1, col, accumulator) + options2Do(row, col - 1, accumulator)
            })

        }
      } else {
        // we know for the same amount of cells, the number of options is the same
        // so if row is greater then columns we compute the mirror
        options2Do(col, row, accumulator)
      }
    }

    options2Do(row, col, MutMap.empty)
  }

  def main(args: Array[String]) {
    (1 to 5).foreach { i => println(options(i, i) + " " + options2(i, i)) }

    (4 to 20).foreach { i => println(s"$i\t${options2(i, i)}\t${new java.util.Date}") }
  }
}
