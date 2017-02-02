package eu.atbk.euler.p000

object P0001 {
  def main(args: Array[String]) {
    val x = (1 until 1000).filter(x => (x % 3 == 0) || (x % 5 == 0)).sum

    println(x)
  }

}
