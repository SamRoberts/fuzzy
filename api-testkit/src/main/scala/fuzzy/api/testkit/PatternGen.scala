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

package fuzzy.api.testkit

import hedgehog.{Gen, Range}
import hedgehog.predef.traverse

import fuzzy.api.Pattern

object PatternGen {
  def literalChar(genChar: Gen[Char]): Gen[Pattern] =
    genChar.map(Pattern.lit)

  def literalChar: Gen[Pattern] =
    literalChar(Gen.alphaNum)

  def unicodeLiteralChar: Gen[Pattern] =
    literalChar(Gen.unicode)

  def any: Gen[Pattern] =
    Gen.constant(Pattern.any)

  def matchChar: Gen[Pattern] =
    Gen.frequency1(9 -> literalChar, 1 -> any)

  def literalString(range: Range[Int]): Gen[Pattern] =
    Gen.list(literalChar, range).map(ps => Pattern.concat(ps))

  def matchString(range: Range[Int]): Gen[Pattern] =
    Gen.list(matchChar, range).map(ps => Pattern.concat(ps))

  def singleKleeneString: Gen[Pattern] =
    matchChar.map(p => Pattern.kleene(p))

  /** range represents number of top level elements in concat pattern. */
  def pattern(range: Range[Int]): Gen[Pattern] =
    Gen.sized { size =>
      if (range.upperBound(size) <= 0)
        matchChar
      else
        Gen.frequency1(
          1 -> patternElement(range.map(_ / 2)),
          3 -> Gen.list(patternElement(range.map(_ / 2)), range).map(ps => Pattern.concat(ps))
        )
    }

  def patternElement(range: Range[Int]): Gen[Pattern] =
    // Note that we don't generate Pattern.kleene(Pattern.concat(...))
    // The pattern ADT has no problem representing such a pattern, but the syntax
    // we currently use can't produce this pattern, so for now we just avoid it.
    for {
      single  <- Gen.frequency1(
                   3 -> literalChar,
                   1 -> any,
                   1 -> pattern(range).map(p => Pattern.group(p))
                 )
      wrapped <- Gen.frequency1(
                   3 -> Gen.constant(single),
                   1 -> Gen.constant(Pattern.kleene(single))
                 )
    } yield wrapped

  def transformPattern(pattern: Pattern, unchangedFrequency: Int, changeFrequency: Int)(change: PartialFunction[Pattern, Gen[Pattern]]): Gen[Pattern] = {
    val transformInner = pattern match {
      case Pattern.Any        => Gen.constant(Pattern.any)
      case p: Pattern.Lit     => Gen.constant(p)
      case Pattern.Group(p)   => transformPattern(p, unchangedFrequency, changeFrequency)(change).map(Pattern.Group(_))
      case Pattern.Kleene(p)  => transformPattern(p, unchangedFrequency, changeFrequency)(change).map(Pattern.Kleene(_))
      case Pattern.Concat(ps) => traverse(ps.toList)(p => transformPattern(p, unchangedFrequency, changeFrequency)(change)).map(Pattern.Concat(_))
    }

    val sometimesChange =
      (pattern: Pattern) =>
        if (change.isDefinedAt(pattern))
          Gen.frequency1(
            changeFrequency    -> change(pattern),
            unchangedFrequency -> Gen.constant(pattern)
          )
        else
          Gen.constant(pattern)

    transformInner.flatMap(sometimesChange)
  }
}
