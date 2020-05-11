package fuzzy

import hedgehog._
import hedgehog.predef._

object PatternGen {
  val kleene     = '*'
  val wildcard   = '.'
  val scopeStart = '('
  val scopeEnd   = ')'

  val controlChars    = List(kleene, scopeStart, scopeEnd)
  val nonLiteralChars = List(kleene, wildcard, scopeStart, scopeEnd)

  val literalChar: Gen[Char] =
    Gen
      .frequency1(9 -> Gen.alphaNum, 1 -> Gen.unicode)
      .filter(c => !nonLiteralChars.contains(c))

  def literalString(range: Range[Int]): Gen[String] =
    Gen.string(literalChar, range)

  val matchChar: Gen[Char] =
    Gen
      .frequency1(9 -> literalChar, 1 -> Gen.constant(wildcard))

  def matchString(range: Range[Int]): Gen[String] =
    Gen.string(matchChar, range)

  val singleKleeneString: Gen[String] =
    matchChar.map(_.toString + kleene.toString)

  def pattern(genString: Gen[String]): Gen[Pattern] = genString.map(Pattern(_))

  def transform(string: String, unchangedFrequency: Int, changeFrequency: Int, change: Char => Gen[String]): Gen[String] =
    traverse(string.toList) { c =>
      Gen.frequency1(unchangedFrequency -> Gen.constant(c.toString), changeFrequency -> change(c))
    }.map(_.mkString)

  def intercalate(
    string: String,
    unchangedFrequency: Int,
    changeFrequency: Int,
    start: Gen[String],
    insert: (Char, Char) => Gen[String],
    end: Gen[String]
  ): Gen[String] = {
    val startFreq = Gen.frequency1(unchangedFrequency -> Gen.constant(""), changeFrequency -> start)
    val endFreq   = Gen.frequency1(unchangedFrequency -> Gen.constant(""), changeFrequency -> end)

    def insertFreq(chars: String) =
      Gen.frequency1(unchangedFrequency -> Gen.constant(""), changeFrequency -> insert(chars(0), chars(1)))

    for {
      strStart   <- startFreq
      strEnd     <- endFreq
      strInserts <- if (string.length < 2) Gen.constant(Nil) else traverse(string.sliding(2).toList)(insertFreq)
    } yield {
      val strChars = string.toList.map(_.toString)
      strStart + (strChars.zipAll(strInserts, "", "").map { case (chr, ins) => chr + ins }.mkString) + strEnd
    }
  }
}
