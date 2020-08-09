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

package fuzzy.matcher.simple

import hedgehog.runner.Properties

import fuzzy.api.{Matcher, Pattern}
import fuzzy.matcher.testkit.{MatcherBoundTests, MatcherComparisonTests, MatcherExactTests}

object SimpleMatcherTest extends MatcherExactTests with MatcherBoundTests {
  override def matcherPatternSizeBound: Int = 2
  override def matcherLiteralSizeBound: Int = 2 
  override def matcherTextSizeBound: Int = 3
  override def matcherAlphabetSizeBound: Int = 30

  override def mkMatcher(pattern: Pattern): Matcher = SimpleMatcher(pattern)
}
