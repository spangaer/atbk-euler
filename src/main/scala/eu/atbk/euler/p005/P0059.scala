package eu.atbk.euler.p005

import scala.collection.immutable.{ Seq => ISeq }
import scala.collection.immutable.{ HashSet => IHashSet }
import eu.atbk.euler.util.MyIO
import java.nio.charset.StandardCharsets.US_ASCII

object P0059 {
  def main(args: Array[String]) {
    // and fits in "max below 127" (original ASCII, maps to UTF8 for first 7 bits) (checked in data)
    val data = MyIO.fileLines("data/p059_cipher.txt").flatMap(_.split(",")).map(java.lang.Byte.parseByte)

    val symbols = data.distinct.sorted

    // frequency analysis
    val freq = symbols.map { s => (s, data.count(_ == s)) }.sortBy(_._2)(Ordering.Int.reverse)
    println(freq)

    val keyTellSelect = freq.take(3).map(_._1)

    // assume the most common symbol is E, e or space
    val tests = ISeq("e", "E", " ").map(_.getBytes(US_ASCII)(0))

    for {
      tell <- keyTellSelect.permutations
      symbol <- tests
    } {
      val key = tell.map(t => t ^ symbol)

      val decrypt = data.zipWithIndex.map { case (b, i) => (b ^ key(i % key.size)).toByte }

      val symbolText = new String(Array(symbol), US_ASCII)
      val text = new String(decrypt.toArray, US_ASCII)

      println(key + "\t\"" + symbolText + "\"")
      println(text)
    }
    // winner is " ", with key ArrayBuffer(103, 111, 100)

    val key = ISeq[Byte](103, 111, 100)
    val decrypt = data.zipWithIndex.map { case (b, i) => (b ^ key(i % key.size)).toByte }

    println(decrypt.map(_.toLong).sum) // 
  }
}
