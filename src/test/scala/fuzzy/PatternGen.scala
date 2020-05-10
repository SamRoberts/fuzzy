package fuzzy

import hedgehog._

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

  val matchChar: Gen[Char] =
    Gen
      .frequency1(9 -> literalChar, 1 -> Gen.constant(wildcard))

  def literalString(range: Range[Int]): Gen[String] =
    Gen.string(literalChar, range)

  val singleKleeneString: Gen[String] =
    matchChar.map(_.toString + kleene.toString)

  def pattern(genString: Gen[String]): Gen[Pattern] = genString.map(Pattern(_))
}
