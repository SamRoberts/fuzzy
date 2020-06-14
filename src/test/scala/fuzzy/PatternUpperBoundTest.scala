package fuzzy

import hedgehog._
import hedgehog.runner._

/** Pattern tests where all we specify are an upper bound on the score
 *
 *  In complex situations, we know how subsets of the pattern should match
 *  subsets of the text, but we don't know if the composition or repetition
 *  of those matches will still be optimal when we build a bigger match
 *  problem out of several simpler match problems.
 *
 *  The tests in this class have this property: we know one way in which a
 *  match could be done, but we aren't sure if it is the optimal match.
 *
 *  However, we can still write a test that asserts the score of the optimal
 *  match is less than or equal to the score of the match we know about.
 *
 *  The more optimal the match we know about, the tighter the test.
 */
object PatternUpperBoundTest extends Properties {
  def tests: List[Test] =
    List(
      property("<abc>* matching a repeated different text",  testArbKleeneMatchDiffRepeated),
      property(". matches more than <a>", testWildcardMatchesMoreThanLiteral)
    )

  def testArbKleeneMatchDiffRepeated: Property = {
    val genBasePatternText = for {
      (charGen, mapper) <- PatternGen.alphabetGenAndMapper(Range.linear(1, 30))
      basePattern       <- Gen.string(charGen, Range.linear(1, 50))
      baseText          <- PatternGen.transformMap(basePattern, 2, 1, mapper)
    } yield (basePattern, baseText)

    for {
      bases          <- genBasePatternText.forAll
      (pattern, text) = bases
      repeat         <- Gen.int(Range.linear(0, 5)).forAll
      result          = Matcher(s"($pattern)*").score(text * repeat)
    } yield {
      val matchScore = repeat * 2 * (text.zip(pattern).count { case (tc,pc) => tc != pc })
      val skipScore  = repeat * text.length
      Result.diffNamed("=== score is worse than matching each repeat individually ===", result.score, matchScore)(_ <= _) and
      Result.diffNamed("=== score is worse than skipping entire text ===", result.score, skipScore)(_ <= _)
    }
  }

  def testWildcardMatchesMoreThanLiteral: Property = {
    // TODO generate any arbitrary complex pattern as long as we can pick out match characters to transform
    for {
      pattern1 <- PatternGen.matchString(Range.linear(0, 100)).forAll
      pattern2 <- PatternGen.transformMap(pattern1, 4, 1, _ => '.').forAll
      text     <- PatternGen.transformChar(pattern1, 2, 1, _ => Gen.unicode).forAll
      result1   = Matcher(pattern1).score(text)
      result2   = Matcher(pattern2).score(text)
    } yield {
      Result.diffNamed("=== Literal pattern score less than wildcard pattern ===", result1.score, result2.score)(_ >= _)
    }
  }
}
