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
import hedgehog.predef._

object StringGen {

  def literal(range: Range[Int]): Gen[String] =
    Gen.string(Gen.alphaNum, range)

  def patternTestString(pattern: Pattern): Gen[String] =
    Gen.choice1(
      Gen.constant(""),
      patternMatchingString(pattern),
      patternMatchingString(pattern).flatMap(s =>
        transform(s, 3, 1, _ => Gen.string(Gen.alphaNum, Range.linear(0, 3)))),
      Gen.string(Gen.alphaNum, Range.linear(0, 16))
    )

  def patternMatchingString(pattern: Pattern): Gen[String] =
    pattern.fold[Gen[String]](
      Gen.alphaNum.map(_.toString),
      c => Gen.constant(c.toString),
      identity[Gen[String]],
      gen => Gen.list(gen, Range.linear(0,4)).map(_.mkString),
      gens => sequence(gens.toList).map(_.mkString)
    )

  /** Returns a generator for literal string patterns, and a mapping from those characters to a separate disjoint character set.
   *
   *  The mapping function is total on characters in the pattern, but is not implemented on other characters.
   *
   *  Alphabet range lower bound should be larger than 0 to ensure we can generate at least some characters.
   */
  def alphabetGenAndMapper(patternRange: Range[Int], alphabetRange: Range[Int]): Gen[(String, Char => Char)] = {
    val alphabetChars = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ "~#_%/,-@").toList
    for {
      uniqChars    <- Gen2.uniqList[Char](alphabetChars, alphabetRange.map(_*2))
      mapping       = uniqChars.sliding(2,2).collect { case List(a,b) => (a,b) }.toList
      (head, tail) <- mapping match {
                        case head :: tail => Gen.constant(head -> tail)
                        case _            => Gen.discard
                      }
      pattern      <- Gen.string(Gen.element(head._1, tail.map(_._1)), patternRange)
    } yield {
      val mapper  = mapping.toMap
      (pattern, mapper)
    }
  }

  def transform(string: String, unchangedFrequency: Int, changeFrequency: Int, change: Char => Gen[String]): Gen[String] =
    traverse(string.toList) { c =>
      Gen.frequency1(unchangedFrequency -> Gen.constant(c.toString), changeFrequency -> change(c))
    }.map(_.mkString)

  def transformChar(string: String, unchangedFrequency: Int, changeFrequency: Int, change: Char => Gen[Char]): Gen[String] =
    transform(string, unchangedFrequency, changeFrequency, (c: Char) => change(c).map(_.toString))

  def transformMap(string: String, unchangedFrequency: Int, changeFrequency: Int, change: Char => Char): Gen[String] =
    transform(string, unchangedFrequency, changeFrequency, (c: Char) => Gen.constant(change(c).toString))

  def transformDel(string: String, unchangedFrequency: Int, deleteFrequency: Int): Gen[String] =
    transform(string, unchangedFrequency, deleteFrequency, (c: Char) => Gen.constant(""))

  def intercalate(
    string: String,
    unchangedFrequency: Int,
    changeFrequency: Int,
    start: Gen[String],
    insert: (Char, Char) => Gen[String],
    end: Gen[String]
  ): Gen[String] = {
    val startFreq = Gen.frequency1(unchangedFrequency -> Gen.constant(""), changeFrequency -> start)
    val endFreq   = Gen.frequency1(unchangedFrequency -> Gen.constant(""), changeFrequency -> end)

    def insertFreq(chars: String) =
      Gen.frequency1(unchangedFrequency -> Gen.constant(""), changeFrequency -> insert(chars(0), chars(1)))

    for {
      strStart   <- startFreq
      strEnd     <- endFreq
      strInserts <- if (string.length < 2) Gen.constant(Nil) else traverse(string.sliding(2).toList)(insertFreq)
    } yield {
      val strChars = string.toList.map(_.toString)
      strStart + (strChars.zipAll(strInserts, "", "").map { case (chr, ins) => chr + ins }.mkString) + strEnd
    }
  }
}
