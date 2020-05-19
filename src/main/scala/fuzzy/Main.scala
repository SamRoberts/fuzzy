package fuzzy

import io.Source

object Main {

  // TODO use decline for CLI
  // TODO use os-lib or see what cats has for file IO, and properly close file too

  def main(args: Array[String]): Unit = {
    val pattern = args(0)
    val file    = args(1)
    val text    = Source.fromFile(file, "UTF-8").mkString

    println(Pattern(pattern).score(text).tracePretty)
  }

}

