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

package fuzzy.matcher.testkit

import hedgehog.{Gen, Property, Range, Syntax}
import hedgehog.runner.{Test, property}

import fuzzy.api.{Matcher, Pattern}
import fuzzy.api.testkit.{PatternGen, StringGen}

/** Simple pattern tests that check the exact match and score of simple pattern building blocks. */
trait MatcherComparisonTests extends StackableProperties {

  def slowestMatcherPatternSizeBound: Int

  def mkMatcher(pattern: Pattern): Matcher
  def mkComparisonMatcher(pattern: Pattern): Matcher

  override def tests: List[Test] =
    super.tests ++ List(
      property("matches comparison matcher on arbitrary pattern and text", testComparisonMatcher)
    )

  def testComparisonMatcher: Property =
    for {
      pattern <- PatternGen.pattern(Range.linear(0, slowestMatcherPatternSizeBound)).forAll
      text     <- StringGen.patternTestString(pattern).forAll
      realRes   = mkMatcher(pattern).score(text)
      testRes   = mkComparisonMatcher(pattern).score(text)
    } yield {
      realRes.score ==== testRes.score
      // an implementation can choose from any valid matched text with the lowest score,
      // so we can't say that matched text must == matched text
    }
}
