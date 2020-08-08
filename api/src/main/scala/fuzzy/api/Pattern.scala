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

package fuzzy.api

import fastparse._, NoWhitespace._
import fastparse.{parse => fastparse}

sealed trait Pattern {

  def flatFold[A](
    any: => A,
    lit: Char => A,
    concat: Seq[A] => A
  ): A =
    fold(any, lit, identity[A], identity[A], concat)

  def fold[A](
    any: => A,
    lit: Char => A,
    group: A => A,
    kleene: A => A,
    concat: Seq[A] => A
  ): A =
    this match {
      case Pattern.Any              => any
      case Pattern.Lit(char)        => lit(char)
      case Pattern.Group(inside)    => group(inside.fold(any, lit, group, kleene, concat))
      case Pattern.Kleene(inside)   => kleene(inside.fold(any, lit, group, kleene, concat))
      case Pattern.Concat(elements) => concat(elements.map(_.fold(any, lit, group, kleene, concat)))
    }
}

object Pattern {

  def any: Pattern                            = Any
  def lit(char: Char): Pattern                = Lit(char)
  def group(inside: Pattern): Pattern         = Group(inside)
  def kleene(inside: Pattern): Pattern        = Kleene(inside)
  def concat(elements: Seq[Pattern]): Pattern = Concat(elements)

  def literal(text: String): Pattern =
    concat(text.map(lit))

  def parse(text: String): Either[String, Pattern] = {
    fastparse(text, parser(_)) match {
      case success : Parsed.Success[Pattern] => Right(success.value)
      case failure : Parsed.Failure          => Left(failure.msg)
    }
  }

  def parser[_: P]: P[Pattern] = {
    val illegalLits = ".*()\\"

    def any: P[Pattern]     = P(".").map(text => Pattern.any)

    def lit: P[Pattern]     = P(CharPred(c => !illegalLits.contains(c)).!).map(text => Pattern.lit(text.head))

    def esc: P[Pattern]     = P("\\" ~/ AnyChar.!).map(text => Pattern.lit(text.head))

    def group: P[Pattern]   = P("(" ~/ pattern ~ ")").map(b => Pattern.group(b))

    def single: P[Pattern]  = P(group | lit | esc | any)

    def elem: P[Pattern]    = (P(single ~ "*".!.?)).map { case (b, Some(_)) => Pattern.kleene(b); case (b, None) => b }

    def pattern: P[Pattern] = elem.rep.map(Pattern.concat(_))

    P(pattern ~ End)
  }

  // NOTE these patterns do not map 1:1 with syntax: multiple syntaxes
  //      can result in the same pattern, and some patterns can't be
  //      expressed via the syntax. I think this is ok, but remember we
  //      must augment with text info to map from pattern to pattern text

  case object Any extends Pattern
  case class Lit(char: Char) extends Pattern
  case class Group(inside: Pattern) extends Pattern
  case class Kleene(inside: Pattern) extends Pattern
  case class Concat(elements: Seq[Pattern]) extends Pattern
}
