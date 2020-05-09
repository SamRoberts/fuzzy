package fuzzy

import hedgehog._
import hedgehog.runner._

object PatternTest extends Properties {

  def tests: List[Test] =
    List(
      property("empty pattern score is text length", testEmptyPatternScoreIsTextLength),
      property("empty pattern match is empty", testEmptyPatternMatchIsEmpty)
    )

  def testEmptyPatternScoreIsTextLength: Property = {
    val pattern = Pattern("")

    for {
      text  <- Gen.string(Gen.unicode, Range.linear(0, 100)).forAll
      result = pattern.score(text)
    } yield {
      result.score ==== text.length
    }
  }

  def testEmptyPatternMatchIsEmpty: Property = {
    val pattern = Pattern("")

    for {
      text  <- Gen.string(Gen.unicode, Range.linear(0, 100)).forAll
      result = pattern.score(text)
    } yield {
      result.matchedText ==== ""
    }
  }
}
