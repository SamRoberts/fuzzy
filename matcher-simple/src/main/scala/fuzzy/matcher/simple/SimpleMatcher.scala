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

import scala.collection.mutable.{Map => MutableMap}

import fuzzy.api.{Match, Matcher, Pattern}

case class SimpleMatcher(pattern: Pattern) extends Matcher {

  def score(text: String): Match = {
    val state = Rest(pattern, text.toList)
    Parser.score(state)
  }
}

case class Steps(score: Int, steps: List[Step]) extends Match {
  def matchedText: String =
    steps.collect { case Step.MatchChar(c) => c }.mkString

  def ++(other: Steps) = Steps(score + other.score, steps ++ other.steps)
}

object Steps {
  def matchChar(c: Char) = Steps(0, List(Step.MatchChar(c)))
  def skipText(c: Char)  = Steps(1, List(Step.SkipText(c)))
  def skipLit(c: Char)   = Steps(1, List(Step.SkipLit(c)))
  val skipAny            = Steps(1, List(Step.SkipAny))
  val enter              = Steps(0, List(Step.Enter))
  val leave              = Steps(0, List(Step.Leave))
  val empty              = Steps(0, Nil)
  val blocked            = Steps(Int.MaxValue / 2, Nil) // TODO be more explicit about being "max score"
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

case class Rest(pattern: List[PStep], text: List[Char])

object Rest {
  def apply(pattern: Pattern, text: List[Char]): Rest =
    Rest(pSteps(pattern), text)

  def pSteps(pattern: Pattern): List[PStep] =
    pattern.fold[List[PStep]](
      List(PStep.Any),
      c => List(PStep.Lit(c)),
      inner => PStep.Enter +: inner :+ PStep.Leave,
      inner => List(PStep.Kleene(inner)),
      _.flatten.toList
    )
}

sealed trait PStep

object PStep {
  case class Lit(c: Char) extends PStep
  case object Any extends PStep
  case object Enter extends PStep
  case object Leave extends PStep
  case class Kleene(inner: List[PStep]) extends PStep

  // ensure we don't loop forever by checking we don't revisit a state 
  case class InfiniteLoopCheck(prevState: Rest) extends PStep
}

object Parser {

  def score(state: Rest): Steps = {
    val f = new RecursiveCachedFunction[Rest, Steps](parse)
    f(state)
  }

  def parse(continuation: Rest => Steps)(state: Rest): Steps = {
    val skipTextBranches  = trySkipText(state)
    val skipPattBranches  = trySkipPattern(state)
    val matchPattBranches = tryMatchPattern(state)

    val allBranches = skipTextBranches ++ skipPattBranches ++ matchPattBranches

    val paths = allBranches.map { case (steps, state) => steps ++ continuation(state) }

    paths match {
      case Nil =>
        state match {
          case Rest(Nil, Nil) => Steps.empty
          case _              => Steps.blocked
        }

      case head :: rest =>
        rest.foldLeft(head)((l: Steps, r: Steps) => if (l.score <= r.score) l else r)
    }
  }

  def trySkipText(state: Rest): List[(Steps, Rest)] =
    state match {
      case Rest(patt, char :: text) => List(Steps.skipText(char) -> Rest(patt, text))
      case _                        => Nil
    }

  def trySkipPattern(state: Rest): List[(Steps, Rest)] =
    state match {
      case Rest(PStep.Any      :: patt, text) => List(Steps.skipAny      -> Rest(patt, text))
      case Rest(PStep.Lit(lit) :: patt, text) => List(Steps.skipLit(lit) -> Rest(patt, text))
      case _                                  => Nil
    }

  def tryMatchPattern(state: Rest): List[(Steps, Rest)] =
    state match {
      case Rest(PStep.Any :: patt, char :: text) =>
        List(
          Steps.matchChar(char) -> Rest(patt, text)
        )

      case Rest(PStep.Lit(lit) :: patt, char :: text) if lit == char =>
        List(
          Steps.matchChar(lit) -> Rest(patt, text)
        )

      case Rest(PStep.Enter :: patt, text) =>
        List(
          Steps.enter -> Rest(patt, text)
        )

      case Rest(PStep.Leave :: patt, text) =>
        List(
          Steps.leave -> Rest(patt, text)
        )

      case Rest(PStep.Kleene(inner) :: patt, text) =>
        val loopPatt =
          inner ++
          List(
            PStep.InfiniteLoopCheck(state), // ensure appending elements to state doesn't cause infinite loop
            PStep.Kleene(inner)
          ) ++
          patt

        List(
          Steps.empty -> Rest(patt, text),
          Steps.empty -> Rest(loopPatt, text)
        )

      case Rest(PStep.InfiniteLoopCheck(prevState) :: patt, text) if Rest(patt, text) != prevState =>
        // TODO Rest(patt, text) != prevState is good enough for now, but allows infinitely growing state
        //      ideally we'd check that state was shrinking, not just that state is changing
        List(
          Steps.empty -> Rest(patt, text)
        )

      case _ =>
        Nil
    }
}

class RecursiveCachedFunction[K,V](f: (K => V) => (K => V)) extends Function[K, V] {
  val cache = MutableMap.empty[K, V]

  def apply(key: K) =
    cache.getOrElseUpdate(key, f(this)(key))
}

