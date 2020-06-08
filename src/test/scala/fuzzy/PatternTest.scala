package fuzzy

import hedgehog._
import hedgehog.runner._

/** Simple pattern tests that check the exact match and score of simple pattern building blocks. */
object PatternTest extends Properties {

  def tests: List[Test] =
    List(
      property("pattern matching simple pattern impl", testSimplePatternImpl),
      property("empty pattern matching arbitrary text", testEmptyMatchArb),
      property("literal pattern matching empty text", testLiteralMatchEmpty),
      property("literal pattern matching the same text", testLiteralMatchSame),
      property("literal pattern matching a different text", testLiteralMatchDifferent),
      property("literal pattern matching a smaller text", testLiteralMatchSubset),
      property("literal pattern matching a larger text", testLiteralMatchSuperset),
      property(".* matching arbitrary text", testWildcardKleeneMatchArb),
      property(".* matching arbitrary subset of text", testWildcardKleeneMatchArbSubset),
      property("<a>* matching arbitrary text with likely some <a>s", testLiteralCharKleeneMatchArb),
      property("(<abc>)* matching repetitions of <abc>", testGroupKleeneMatchSameRepeated)
    )

  def testSimplePatternImpl: Property =
    for {
      testPatt <- PatternGen.implPattern(Range.linear(0, 8)).forAll
      realPatt  = Pattern(testPatt.toString) // TODO share pattern construction code, maybe a type class construction?
      text     <- PatternGen.implPatternTestString(testPatt).forAll
      testRes   = testPatt.score(text)
      realRes   = realPatt.score(text)
    } yield {
      realRes.score ==== testRes.score
      // an implementation can choose from any valid matched text with the lowest score,
      // so we can't say that matched text must == matched text
    }

  def testEmptyMatchArb: Property = {
    val pattern = Pattern("")

    for {
      text  <- Gen.string(Gen.unicode, Range.linear(0, 100)).forAll
      result = pattern.score(text)
    } yield {
      (result.score ==== text.length) and
      (result.matchedText ==== "")
    }
  }

  def testLiteralMatchEmpty: Property = {
    for {
      pattern <- PatternGen.literalString(Range.linear(0, 100)).forAll
      result   = Pattern(pattern).score("")
    } yield {
      (result.score ==== pattern.length) and
      (result.matchedText ==== "")
    }
  }

  def testLiteralMatchSame: Property = {
    for {
      pattern <- PatternGen.literalString(Range.linear(0, 100)).forAll
      result   = Pattern(pattern).score(pattern)
    } yield {
      (result.score ==== 0) and
      (result.matchedText ==== pattern)
    }
  }

  def testLiteralMatchDifferent: Property = {
      // the algorithm will take a penalty of 2 to overcome a single character difference
      // however, it may do better if two consecutive characters change in such a way that the
      // overall string pair effectively only has a single addition or subtraction
      // so this test must ensure that changed characters are different to original characters
    val gen = for {
      (charGen, mapper) <- PatternGen.alphabetGenAndMapper(Range.linear(1, 30))
      pattern           <- Gen.string(charGen, Range.linear(0, 100))
      text              <- PatternGen.transformMap(pattern, 2, 1, mapper)
    } yield (pattern, text)

    for {
      patternAndText <- gen.forAll
      (pattern, text) = patternAndText
      result          = Pattern(pattern).score(text)
    } yield {
      val expectedMatch = text.zip(pattern).collect { case (tc,pc) if tc == pc => tc }.mkString
      (result.score ==== 2*(text.length - expectedMatch.length)) and
      (result.matchedText ==== expectedMatch)
    }
  }

  def testLiteralMatchSubset: Property = {
    for {
      pattern <- PatternGen.literalString(Range.linear(0, 100)).forAll
      text    <- PatternGen.transformDel(pattern, 2, 1).forAll
      result   = Pattern(pattern).score(text)
    } yield {
      (result.score ==== (pattern.length - text.length)) and
      (result.matchedText ==== text)
    }
  }

  def testLiteralMatchSuperset: Property = {
    val insertedString = PatternGen.matchString(Range.singleton(1))
    for {
      pattern <- PatternGen.literalString(Range.linear(0, 100)).forAll
      text    <- PatternGen.intercalate(
                   pattern, 2, 1,
                   insertedString,
                   (_, _) => insertedString,
                   insertedString
                 ).forAll
      result   = Pattern(pattern).score(text)
    } yield {
      (result.score ==== (text.length - pattern.length)) and
      (result.matchedText ==== pattern)
    }
  }

  def testWildcardKleeneMatchArb: Property = {
    val pattern = Pattern(".*")

    for {
      text  <- Gen.string(Gen.unicode, Range.linear(0, 100)).forAll
      result = pattern.score(text)
    } yield {
      (result.score ==== 0) and
      (result.matchedText ==== text)
    }
  }

  def testWildcardKleeneMatchArbSubset: Property = {
    for {
      prefix     <- PatternGen.literalString(Range.linear(0, 50)).forAll
      suffix     <- PatternGen.literalString(Range.linear(0, 50)).forAll
      textMiddle <- Gen.string(Gen.unicode, Range.linear(0, 100)).forAll
      pattern     = prefix + ".*" + suffix
      text        = prefix + textMiddle + suffix
      result      = Pattern(pattern).score(text)
    } yield {
      (result.score ==== 0) and
      (result.matchedText ==== text)
    }
  }

  def testLiteralCharKleeneMatchArb: Property = {
    for {
      literal <- PatternGen.literalChar.forAll
      text    <- Gen.string(Gen.frequency1(9 -> Gen.unicode, 1 -> Gen.constant(literal)), Range.linear(0, 100)).forAll
      pattern  = Pattern(literal.toString + PatternGen.kleene)
      result   = pattern.score(text)
    } yield {
      val matchLength = text.count(_ == literal)
      (result.score ==== (text.length - matchLength)) and
      (result.matchedText ==== literal.toString * matchLength)
    }
  }
  def testGroupKleeneMatchSameRepeated: Property = {
    for {
      literal <- PatternGen.literalString(Range.linear(0, 50)).forAll
      repeat  <- Gen.int(Range.linear(0, 5)).forAll
      text     = literal * repeat
      result   = Pattern(s"($literal)*").score(text)
    } yield {
      (result.score ==== 0) and
      (result.matchedText ==== text)
    }
  }
}
