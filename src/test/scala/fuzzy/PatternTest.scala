package fuzzy

import hedgehog._
import hedgehog.runner._

object PatternTest extends Properties {

  def tests: List[Test] =
    List(
      property("empty pattern matching arbitrary text", testEmptyMatchArb),
      property("literal pattern matching empty text", testLiteralMatchEmpty),
      property("literal pattern matching the same text", testLiteralMatchSame),
      property("literal pattern matching a different text", testLiteralMatchDifferent),
      property("literal pattern matching a smaller text", testLiteralMatchSubset),
      property("literal pattern matching a larger text", testLiteralMatchSuperset),
      property(".* matching arbitrary text", testWildcardKleeneMatchArb),
      property(".* matching arbitrary subset of text", testWildcardKleeneMatchArbSubset),
      property("<a>* matching arbitrary text with likely some <a>s", testLiteralCharKleeneMatchArb),
      property("<abc>* matching a repeated different text",  testArbKleeneMatchRepeats),
      property(". matches more than <a>", testWildcardMatchesMoreThanLiteral)
    )

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
      (result.matchedText ==== literal.toString.repeat(matchLength))
    }
  }

  def testArbKleeneMatchRepeats: Property = {
    val genBasePatternText = for {
      (charGen, mapper) <- PatternGen.alphabetGenAndMapper(Range.linear(1, 30))
      basePattern       <- Gen.string(charGen, Range.linear(1, 100))
      baseText          <- PatternGen.transformMap(basePattern, 2, 1, mapper)
    } yield (basePattern, baseText)

    for {
      bases          <- genBasePatternText.forAll
      (pattern, text) = bases
      repeat         <- Gen.int(Range.linear(0, 10)).forAll
      result          = Pattern(s"($pattern)*").score(text * repeat)
    } yield {
      val matchText  = text.zip(pattern).collect { case (tc,pc) if tc == pc => tc }.mkString
      val matchScore = 2 * (text.length - matchText.length)
      val skipText   = ""
      val skipScore  = text.length
      println("vvvvvvvvvvvvvvvvvvvvvvvvvvv")
      println("base pattern = [" + pattern + "], length " + pattern.length)
      println("base text = [" + text + "], length " + text.length)
      println("repeat = [" + repeat + "]")
      println("result.match = [" + result.matchedText + "], length " + result.matchedText.length)
      println("result.score = [" + result.score + "]")
      println("^^^^^^^^^^^^^^^^^^^^^^^^^^^")
      (result.score ==== (matchScore min skipScore) * repeat) and
      ((result.matchedText ==== matchText * repeat) or (result.matchedText ==== skipText * repeat))
    }
  }

  def testWildcardMatchesMoreThanLiteral: Property = {
    // TODO generate any arbitrary complex pattern as long as we can pick out match characters to transform
    for {
      pattern1 <- PatternGen.matchString(Range.linear(0, 100)).forAll
      pattern2 <- PatternGen.transformMap(pattern1, 4, 1, _ => '.').forAll
      text     <- PatternGen.transformChar(pattern1, 2, 1, _ => Gen.unicode).forAll
      result1   = Pattern(pattern1).score(text)
      result2   = Pattern(pattern2).score(text)
    } yield {
      Result.diffNamed("=== Literal pattern score less than wildcard pattern ===", result1.score, result2.score)(_ >= _)
    }
  }
}
