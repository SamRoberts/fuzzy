package fuzzy

import hedgehog._
import hedgehog.runner._

object PatternTest extends Properties {

  def tests: List[Test] =
    List(
      property("empty pattern matching arbitrary text", testEmptyMatchArb),
      property("literal pattern matching empty text", testLiteralMatchEmpty),
      property(".* matching arbitrary text", testWildcardKleeneMatchArb),
      property("<a>* matching arbitrary text with likely some <a>s", testLiteralCharKleeneMatchArb)
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
      pattern <- PatternGen.pattern(PatternGen.literalString(Range.linear(0, 100))).forAll
      result   = pattern.score("")
    } yield {
      (result.score ==== pattern.pattern.length) and
      (result.matchedText ==== "")
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
}