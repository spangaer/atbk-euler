package eu.atbk.euler.p003

import eu.atbk.euler.util.MyMath

import scala.collection.immutable.{ Seq => ISeq }
import scala.collection.immutable.{ HashSet => IHashSet }

object P0035 {

  def main(args: Array[String]) {

    val data = MyMath.primesBelow1million

    val lookUp = data.to[IHashSet]

    val res = data.seq.filter { p =>
      val split = MyMath.numberToDigitSeq(p)

      if (split.contains(0)) { // zeros destroy cycles
        false
      } else {
        MyMath.rotations(split)
          .map(MyMath.digitSeqToNumber(_))
          .forall { x =>
            println(x)
            val out = lookUp.contains(x)
            out
          }
      }
    }.seq

    println(res)
    println(res.size) // 55

  }

}
