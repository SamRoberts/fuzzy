// Copyright 2021 Sam Roberts
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package fuzzy.matcher.testkit

import hedgehog.{Gen, Property, Range, Result, Syntax}
import hedgehog.runner.{Test, property}

import fuzzy.api.{Matcher, Pattern}
import fuzzy.api.testkit.{PatternGen, StringGen}

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
trait MatcherBoundTests extends StackableProperties {
  def matcherPatternSizeBound: Int
  def matcherTextSizeBound: Int
  def matcherAlphabetSizeBound: Int

  def mkMatcher(pattern: Pattern): Matcher

  override def tests: List[Test] =
    super.tests ++ List(
      property("<abc>* matching a repeated different text",  testArbKleeneMatchDiffRepeated),
      property(". matches more than <a>", testWildcardMatchesMoreThanLiteral),
      property("<NO PROGRESS>* are handled correctly (empty text only)", testInfiniteLoopCheck)
    )

  def testArbKleeneMatchDiffRepeated: Property =
    for {
      literalMapper     <- StringGen.alphabetGenAndMapper(
                             Range.linear(1, matcherTextSizeBound / 4),
                             Range.linear(1, matcherAlphabetSizeBound)
                           ).forAll
      (literal, mapper)  = literalMapper
      baseText          <- StringGen.transformMap(literal, 2, 1, mapper).forAll
      repeat            <- Gen.int(Range.linear(0, 5)).forAll
      result             = mkMatcher(Pattern.kleene(Pattern.literal(literal))).score(baseText * repeat)
    } yield {
      val matchScore = repeat * 2 * (literal.zip(baseText) count { case (tc,pc) => tc != pc })
      val skipScore  = repeat * baseText.length
      Result.diffNamed("=== score is worse than matching each repeat individually ===", result.score, matchScore)(_ <= _) and
      Result.diffNamed("=== score is worse than skipping entire text ===", result.score, skipScore)(_ <= _)
    }

  def testWildcardMatchesMoreThanLiteral: Property = {
    for {
      pattern1 <- PatternGen.pattern(Range.linear(0, matcherPatternSizeBound)).forAll
      pattern2 <- PatternGen.transformPattern(pattern1, 2, 1) { case Pattern.Lit(_) => Gen.constant(Pattern.any) }.forAll
      text     <- StringGen.patternTestString(pattern1).forAll
      result1   = mkMatcher(pattern1).score(text)
      result2   = mkMatcher(pattern2).score(text)
    } yield {
      Result.diffNamed("=== Literal pattern score less than wildcard pattern ===", result1.score, result2.score)(_ >= _)
    }
  }

  def testInfiniteLoopCheck: Property =
    // TODO this test is meant to trigger bugs in the simple matcher infinite loop short cut
    //      this highly specific test gen is probably missing a lot of valuable related cases
    //      there is probably a broader underlying test for "bad kleenes" we could get at
    for {
      literalMapper     <- StringGen.alphabetGenAndMapper(
                             Range.linear(1, matcherTextSizeBound / 4),
                             Range.linear(1, matcherAlphabetSizeBound)
                           ).forAll
      (literal, mapper)  = literalMapper
      basePattern        = Pattern.literal(literal)
      kleenePattern     <- PatternGen.transformPattern(basePattern, 2, 1) {
                             case Pattern.Lit(c) => Gen.constant(Pattern.Kleene(Pattern.Lit(mapper(c))))
                           }.forAll
      noKleenePattern   <- PatternGen.transformPattern(kleenePattern, 1, 3) {
                             case Pattern.Kleene(_) => Gen.constant(Pattern.Concat(List()))
                           }.forAll
      kleeneResult       = mkMatcher(kleenePattern).score("")
      noKleeneResult     = mkMatcher(noKleenePattern).score("")
    } yield {
      Result.diffNamed("=== Kleene patterns which matched nothing changed score ===", kleeneResult.score, noKleeneResult.score)(_ == _)
    }
}

