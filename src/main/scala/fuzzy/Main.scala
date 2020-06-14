package fuzzy

import io.Source

object Main {

  // TODO use decline for CLI
  // TODO use os-lib or see what cats has for file IO, and properly close file too

  def main(args: Array[String]): Unit = {
    val patternFile = args(0)
    val textFile    = args(1)
    val pattern     = Source.fromFile(patternFile, "UTF-8").mkString
    val text        = Source.fromFile(textFile, "UTF-8").mkString

    println(Matcher(pattern).score(text).matchedText)
  }

}

