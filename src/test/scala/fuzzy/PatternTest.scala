package fuzzy

import hedgehog._
import hedgehog.runner._

object PatternTest extends Properties {

  def tests: List[Test] =
    List(
      property("empty pattern matching arbitrary text", testEmptyMatchArb),
      property("literal pattern matching empty text", testLiteralMatchEmpty),
      property(".* matching arbitrary text", testWildcardKleeneMatchArb)
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
}
