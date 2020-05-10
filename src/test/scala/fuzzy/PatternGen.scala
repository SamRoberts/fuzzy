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

  def transform(string: String, unchangedFrequency: Int, changeFrequency: Int, change: Char => Gen[Char]): Gen[String] =
    traverse(string.toList)(c => Gen.frequency1(unchangedFrequency -> Gen.constant(c), changeFrequency -> change(c))).map(_.mkString)
}
