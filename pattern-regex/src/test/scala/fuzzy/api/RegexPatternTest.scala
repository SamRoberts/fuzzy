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

package fuzzy.pattern.regex

import hedgehog.{Gen, Property, Range, Result, Syntax}
import hedgehog.runner.{Properties, Test, property}

import fuzzy.api.Pattern
import fuzzy.api.testkit.{PatternGen, StringGen}

/** Simple pattern tests that check the exact match and score of simple pattern building blocks. */
object RegexPatternTest extends Properties {

  def tests: List[Test] =
    List(
      property("canonical pattern roundtrip", testCanonicalPatternRoundtrip),
      property("literal pattern parses correctly", testLiteralPattern),
      property("mismatched brackets fails", testMismatchedBrackets)
    )

  // for the purposes of testing the pattern parser, we introduce a pattern printer which
  // prints patterns out in canonical form
  def print(pattern: Pattern): String = {
    val illegalLits = ".*()\\"
    pattern match {
      case Pattern.Any => "."
      case Pattern.Lit(c) => if (illegalLits.contains(c)) s"\\$c" else s"$c"
      case Pattern.Group(inner) => s"(${print(inner)})"
      case Pattern.Kleene(Pattern.Any) => s"${print(Pattern.Any)}*"
      case Pattern.Kleene(inner: Pattern.Group) => s"${print(inner)}*"
      case Pattern.Kleene(inner) => s"(${print(inner)})*"
      case Pattern.Concat(inners) => inners.map(print).mkString
    }
  }

  def testCanonicalPatternRoundtrip: Property =
    for {
      pattern <- PatternGen.pattern(Range.linear(0, 12)).forAll
    } yield {
      val canonical = RegexPattern.parse(print(pattern))
      val roundtrip = canonical.right.flatMap(p => RegexPattern.parse(print(p)))

      Result.assert(canonical.isRight) and // bad error message if this fails, but should never fail
      (roundtrip ==== canonical)
    }

  def testLiteralPattern: Property =
    for {
      patternStr <- StringGen.literal(Range.linear(0, 100)).forAll
    } yield {
      val pattern1 = RegexPattern.parse(patternStr)
      val pattern2 = Pattern.literal(patternStr)
      pattern1 ==== Right(pattern2)
    }

  def testMismatchedBrackets: Property = {
    def isMismatched(pattern: String): Boolean = {
      val depth = pattern.scanLeft(0) { (d: Int, c: Char) => c match {
                    case '(' => d + 1
                    case ')' => d - 1
                    case  _  => d
                  }}

      depth.exists(_ < 0) || depth.last != 0
    }

    for {
      patternStr <- Gen
                      .string(
                        Gen.frequency1(
                          4 -> Gen.alphaNum,
                          1 -> Gen.constant(')'),
                          1 -> Gen.constant('(')
                        ),
                        Range.linear(0, 100)
                      )
                      .filter(isMismatched)
                      .forAll
    } yield {
      val pattern = RegexPattern.parse(patternStr)
      Result.assert(pattern.isLeft)
    }
  }
}
