// Copyright 2020 Sam Roberts
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

  def testArbKleeneMatchDiffRepeated: Property =
    for {
      literalMapper     <- StringGen.alphabetGenAndMapper(Range.linear(1, 50), Range.linear(1, 30)).forAll
      (literal, mapper)  = literalMapper
      baseText          <- StringGen.transformMap(literal, 2, 1, mapper).forAll
      repeat            <- Gen.int(Range.linear(0, 5)).forAll
      result             = Matcher(Pattern.kleene(Pattern.literal(literal))).score(baseText * repeat)
    } yield {
      val matchScore = repeat * 2 * (literal.zip(baseText) count { case (tc,pc) => tc != pc })
      val skipScore  = repeat * baseText.length
      Result.diffNamed("=== score is worse than matching each repeat individually ===", result.score, matchScore)(_ <= _) and
      Result.diffNamed("=== score is worse than skipping entire text ===", result.score, skipScore)(_ <= _)
    }

  def testWildcardMatchesMoreThanLiteral: Property = {
    for {
      pattern1 <- PatternGen.pattern(Range.linear(0, 8)).forAll
      pattern2 <- PatternGen.transformPattern(pattern1, 2, 1) { case Pattern.Lit(_) => Gen.constant(Pattern.any) }.forAll
      text     <- StringGen.patternTestString(pattern1).forAll
      result1   = Matcher(pattern1).score(text)
      result2   = Matcher(pattern2).score(text)
    } yield {
      Result.diffNamed("=== Literal pattern score less than wildcard pattern ===", result1.score, result2.score)(_ >= _)
    }
  }
}
