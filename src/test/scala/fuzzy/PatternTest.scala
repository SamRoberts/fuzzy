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

// TODO avoid constant need to disambiguate these classes, with a common interface or using different class names
import fuzzy.impl.{Matcher => ImplMatcher}

/** Simple pattern tests that check the exact match and score of simple pattern building blocks. */
object PatternTest extends Properties {

  def tests: List[Test] =
    List(
      property("pattern matching simple pattern impl", testSimplePatternImpl),
      property("empty pattern matching arbitrary text", testEmptyMatchArb),
      property("literal pattern matching empty text", testLiteralMatchEmpty),
      property("literal pattern matching the same text", testLiteralMatchSame),
      property("literal pattern matching a different text", testLiteralMatchDifferent),
      property("literal pattern matching a smaller text", testLiteralMatchSubset),
      property("literal pattern matching a larger text", testLiteralMatchSuperset),
      property(".* matching arbitrary text", testWildcardKleeneMatchArb),
      property(".* matching arbitrary subset of text", testWildcardKleeneMatchArbSubset),
      property("<a>* matching arbitrary text with likely some <a>s", testLiteralCharKleeneMatchArb),
      property("(<abc>)* matching repetitions of <abc>", testGroupKleeneMatchSameRepeated)
    )

  def testSimplePatternImpl: Property =
    for {
      pattern <- PatternGen.pattern(Range.linear(0, 4)).forAll
      text     <- StringGen.patternTestString(pattern).forAll
      testRes   = ImplMatcher.score(pattern, text)
      realRes   = Matcher(pattern).score(text) // TODO share pattern for calling API, maybe literally with a common interface
    } yield {
      realRes.score ==== testRes.score
      // an implementation can choose from any valid matched text with the lowest score,
      // so we can't say that matched text must == matched text
    }

  def testEmptyMatchArb: Property = {
    val pattern = Matcher(Pattern.literal(""))

    for {
      text  <- StringGen.literal(Range.linear(0, 100)).forAll
      result = pattern.score(text)
    } yield {
      (result.score ==== text.length) and
      (result.matchedText ==== "")
    }
  }

  def testLiteralMatchEmpty: Property = {
    for {
      literal <- StringGen.literal(Range.linear(0, 100)).forAll
      result   = Matcher(Pattern.literal(literal)).score("")
    } yield {
      (result.score ==== literal.length) and
      (result.matchedText ==== "")
    }
  }

  def testLiteralMatchSame: Property = {
    for {
      literal <- StringGen.literal(Range.linear(0, 100)).forAll
      result   = Matcher(Pattern.literal(literal)).score(literal)
    } yield {
      (result.score ==== 0) and
      (result.matchedText ==== literal)
    }
  }

  def testLiteralMatchDifferent: Property = {
    // the algorithm will take a penalty of 2 to overcome a single character difference
    // however, it may do better if two consecutive characters change in such a way that the
    // overall string pair effectively only has a single addition or subtraction
    // so this test must ensure that changed characters are different to original characters

    for {
      literalMapper     <- StringGen.alphabetGenAndMapper(Range.linear(0, 100), Range.linear(1, 30)).forAll
      (literal, mapper)  = literalMapper
      text              <- StringGen.transformMap(literal, 2, 1, mapper).forAll
      result             = Matcher(Pattern.literal(literal)).score(text)
    } yield {
      val expectedMatch = text.zip(literal).collect { case (tc,pc) if tc == pc => tc }.mkString
      (result.score ==== 2*(text.length - expectedMatch.length)) and
      (result.matchedText ==== expectedMatch)
    }
  }

  def testLiteralMatchSubset: Property = {
    for {
      literal <- StringGen.literal(Range.linear(0, 100)).forAll
      text    <- StringGen.transformDel(literal, 2, 1).forAll
      result   = Matcher(Pattern.literal(literal)).score(text)
    } yield {
      (result.score ==== (literal.length - text.length)) and
      (result.matchedText ==== text)
    }
  }

  def testLiteralMatchSuperset: Property = {
    val insertedString = StringGen.literal(Range.singleton(1))
    for {
      literal <- StringGen.literal(Range.linear(0, 100)).forAll
      text    <- StringGen.intercalate(
                   literal, 2, 1,
                   insertedString,
                   (_, _) => insertedString,
                   insertedString
                 ).forAll
      result   = Matcher(Pattern.literal(literal)).score(text)
    } yield {
      (result.score ==== (text.length - literal.length)) and
      (result.matchedText ==== literal)
    }
  }

  def testWildcardKleeneMatchArb: Property = {
    val matcher = Matcher(Pattern.kleene(Pattern.any))

    for {
      text  <- StringGen.literal(Range.linear(0, 100)).forAll
      result = matcher.score(text)
    } yield {
      (result.score ==== 0) and
      (result.matchedText ==== text)
    }
  }

  def testWildcardKleeneMatchArbSubset: Property = {
    for {
      prefix     <- StringGen.literal(Range.linear(0, 50)).forAll
      suffix     <- StringGen.literal(Range.linear(0, 50)).forAll
      textMiddle <- StringGen.literal(Range.linear(0, 100)).forAll
      pattern     = Pattern.concat(List(Pattern.literal(prefix), Pattern.kleene(Pattern.any), Pattern.literal(suffix)))
      text        = prefix + textMiddle + suffix
      result      = Matcher(pattern).score(text)
    } yield {
      (result.score ==== 0) and
      (result.matchedText ==== text)
    }
  }

  def testLiteralCharKleeneMatchArb: Property = {
    for {
      litChar <- Gen.alphaNum.forAll
      text    <- Gen.string(Gen.frequency1(9 -> Gen.alphaNum, 1 -> Gen.constant(litChar)), Range.linear(0, 100)).forAll
      pattern  = Matcher(Pattern.kleene(Pattern.literal(litChar.toString)))
      result   = pattern.score(text)
    } yield {
      val matchLength = text.count(_ == litChar)
      (result.score ==== (text.length - matchLength)) and
      (result.matchedText ==== litChar.toString * matchLength)
    }
  }

  def testGroupKleeneMatchSameRepeated: Property = {
    for {
      literal <- StringGen.literal(Range.linear(0, 50)).forAll
      repeat  <- Gen.int(Range.linear(0, 5)).forAll
      text     = literal * repeat
      result   = Matcher(Pattern.kleene(Pattern.literal(literal))).score(text)
    } yield {
      (result.score ==== 0) and
      (result.matchedText ==== text)
    }
  }
}
