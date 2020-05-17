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

  def literalLargeChar: Gen[Char] =
    Gen.unicode.filter(c => !nonLiteralChars.contains(c))

  def literalString(range: Range[Int]): Gen[String] =
    Gen.string(literalChar, range)

  /** Returns a generator for characters from an arbitrary alphabet, and a mapping from thse characters to others.
   *
   *  All characters and mapped characters are unique.
   *
   *  The mapping function is total on characters from the alphabet, but is not implemented on other characters.
   *
   *  The alphabet will have at least one character. Range lower bound should be largeer than 0.
   */
  def alphabetGenAndTranslation(range: Range[Int]): Gen[(Gen[Char], Char => Char)] =
    for {
      uniqChars    <- Gen2.uniqList[Char](PatternGen.literalLargeChar, range.map(_*2))
      mapping       = uniqChars.sliding(2,2).map { case List(a,b) => (a,b) }.toList
      (head, tail) <- mapping match {
                        case head :: tail => Gen.constant(head ->tail)
                        case _            => Gen.discard
      }
      alphabetGen   = Gen.element(head._1, tail.map(_._1))
      mapFunction   = mapping.toMap
    } yield {
      (alphabetGen, mapFunction)
    }

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
