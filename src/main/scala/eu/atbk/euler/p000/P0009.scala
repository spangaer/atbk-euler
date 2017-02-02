package eu.atbk.euler.p000

import scala.annotation.tailrec

object P0009 {
  def main(args: Array[String]) {

    println(pytatrip_breakInner((3, 4, 5)))

    P0009(12) // (3,4,5) : 60

    (1 to 1000).foreach { i =>
      print(s"${i} ")
      P0009(i)
    }
    //P0009(100) // (,,) : 
  }

  def apply(i: Int) {
    val aSeek = i / 3

    def bSeek(a: Long) = (i - a) / 2

    @tailrec
    def seek(a: Long = aSeek, b: Long = bSeek(aSeek)): Option[(Long, Long, Long)] = {
      val c = i - a - b
      val trip = (a, b, c)
      val (pytatrip, breakInner) = pytatrip_breakInner(trip)
      if (pytatrip)
        Some(trip)
      else if (a <= 1 && (b <= 2 || breakInner)) {
        None
      } else if (breakInner || (a + 1 >= b)) {
        val newA = a - 1;
        val newB = bSeek(newA)
        seek(newA, newB)
      } else {
        val newB = b - 1
        seek(a, newB)
      }
    }

    seek() match {
      case Some(trip @ (a, b, c)) =>
        println(s"${trip} ${a * b * c}")
      case None =>
        println("nope")
    }

  }

  def pytatrip_breakInner(trip: (Long, Long, Long)): (Boolean, Boolean) =
    trip match {
      case (a, b, c) =>
        val (as, bs, cs) = (a * a, b * b, c * c)

        ((a < b) && (b < c) && (as + bs == cs), (as + bs <= cs))
    }

}
