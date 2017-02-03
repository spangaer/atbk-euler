package eu.atbk.euler.util

import java.nio.file.Files
import java.nio.file.Paths
import java.nio.charset.StandardCharsets
import scala.collection.JavaConverters._

object MyIO {
  def string2NumberSeqSeq(in: String): Seq[Seq[Long]] = {
    in.split(Array('\r', '\n')).filterNot(_.isEmpty())
      .map(row => row.split(" ").filterNot(_.isEmpty()).map(java.lang.Long.parseLong(_)).toSeq).toSeq
  }

  def file2NumberSeqSeq(in: String): Seq[Seq[Long]] = {
    Files.readAllLines(Paths.get(in), StandardCharsets.UTF_8).asScala
      .map(row => row.split(" ").filterNot(_.isEmpty()).map(java.lang.Long.parseLong(_)).toSeq).toSeq
  }
}
