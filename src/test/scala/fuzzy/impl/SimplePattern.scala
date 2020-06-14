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

package fuzzy.impl

import fuzzy.PatternFactory

object Matcher {

  import Element._
  import Arity._
  import Step._

  def score(pattern: Pattern, text: String): Match =
    score(pattern.elements, text.toList, Nil, false)

  def score(pattern: List[Element], text: List[Char], env: List[Scope], progressed: Boolean): Match = (pattern, env, text) match {
    case (Nil,            Nil,       Nil)           => Match(Nil)
    case (Nil,            Nil, t :: rest)           => SkipText(t)  +: score(Nil, rest, env, progressed)

    case (Lit(p) :: next, env, Nil      )           => SkipLit(p)   +: score(next, Nil, env, progressed)
    case (Lit(p) :: next, env, t :: rest) if p == t => MatchChar(p) +: score(next, rest, env, true)
    case (Lit(p) :: next, env, t :: rest) if p != t => (SkipLit(p)  +: score(next, t :: rest, env, progressed)) or
                                                       (SkipText(t) +: score(Lit(p) :: next, rest, env, true))

    case (Any :: next,    env, Nil      )           => SkipAny +: score(next, Nil, env, progressed)
    case (Any :: next,    env, t :: rest)           => MatchChar(t) +: score(next, rest, env, true)

    case (Push(One,  start) :: after, env, text)    => Enter +: score(start, text, Scope(One,  start, after, progressed) :: env, false)
    case (Push(Many, start) :: after, env, text)    => (Enter +: score(start, text, Scope(Many, start, after, progressed) :: env, false)) or
                                                       score(after, text, env, progressed)

    case (Nil, Scope(One,  _,     after, afterProgressed) :: env, text)               =>
      Leave +: score(after, text, env, afterProgressed)

    case (Nil, Scope(Many, _,     after, afterProgressed) :: env, text) if !progressed =>
      Leave +: score(after, text, env, afterProgressed)

    case (Nil, Scope(Many, start, after, afterProgressed) :: env, text) if progressed  =>
      score(start, text, Scope(Many, start, after, afterProgressed) :: env, false) or
      (Leave +: score(after, text, env, afterProgressed))
  }
}

case class Match(steps: List[Step]) {

  import Step._

  def score: Int =
    steps.map {
      case _: MatchChar => 0
      case _: SkipText  => 1
      case _: SkipLit   => 1
      case SkipAny      => 1
      case Enter        => 0
      case Leave        => 0
    }.sum

  // TODO share match API with real implementation
  def matchedText: String =
    steps.collect { case MatchChar(c) => c }.mkString

  def or(other: Match) = if (score <= other.score) this else other

  def +:(step: Step): Match = Match(step +: steps)
}

sealed trait Step

object Step {
  case class MatchChar(c: Char) extends Step
  case class SkipText(c: Char) extends Step
  case class SkipLit(c: Char) extends Step
  case object SkipAny extends Step
  case object Enter extends Step
  case object Leave extends Step
}

case class Scope(arity: Arity, start: List[Element], after: List[Element], afterProgressed: Boolean)

sealed trait Arity

object Arity {
  case object One extends Arity
  case object Many extends Arity
}

sealed trait Element

object Element {
  case class Lit(char: Char) extends Element
  case object Any extends Element
  case class Push(arity: Arity, start: List[Element]) extends Element
}

case class Pattern(elements: List[Element]) {
  import Arity._
  import Element._

  override def toString: String =
    elements.map {
      case Lit(c) => c.toString
      case Any    => "."
      case Push(a, es) =>
        val inner = Pattern(es).toString

        if (inner.size == 1 && a == Many) s"${inner}*"
        else if (a == Many)               s"(${inner})*"
        else                              s"(${inner})"
    }.mkString
}

object Pattern {

  import Element._
  import Arity._
  import Step._

  implicit val factory = new PatternFactory[Pattern] {
    type Builder = List[Element]
    def freeze(builder: Builder): Pattern = Pattern(builder)

    def any(text: String): Builder =
      List(Any)

    def lit(char: Char, text: String): Builder =
      List(Lit(char))

    def group(inside: Builder, textPre: String, textPost: String): Builder =
      List(Push(One, inside))

    def kleene(inside: Builder, testPre: String, textPost: String): Builder =
      inside match {
        case List(Push(One, inner)) => List(Push(Many, inner))
        case _                      => List(Push(Many, inside))
      }

    def concat(builders: Seq[Builder]): Builder =
      builders.flatten.toList
  }
}

