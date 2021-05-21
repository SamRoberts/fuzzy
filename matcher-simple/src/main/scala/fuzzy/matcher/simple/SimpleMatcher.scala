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

import cats.{Alternative, Eval, Functor, Monad, Monoid}
import cats.data.{IndexedStateT, Nested, ReaderT, StateT, Writer}
import cats.mtl.{Ask, Stateful, Tell}
import cats.implicits._

import fs2.Stream

import cats.instances.function._

import fuzzy.api.{Match, Matcher, Pattern}

import java.util.HashMap

case class Result[A](steps: List[Step], rest: List[Char], value: A)

case class Steps(score: Int, steps: List[Step])

object Steps {
  def matchChar(c: Char) = Steps(0, List(Step.MatchChar(c)))
  def skipText(c: Char)  = Steps(1, List(Step.SkipText(c)))
  def skipLit(c: Char)   = Steps(1, List(Step.SkipLit(c)))
  val skipAny            = Steps(1, List(Step.SkipAny))
  val enter              = Steps(0, List(Step.Enter))
  val leave              = Steps(0, List(Step.Leave))

  implicit val stepsMonoid = new Monoid[Steps] {
    def combine(x: Steps, y: Steps) = Steps(x.score + y.score, x.steps ++ y.steps)
    def empty                       = Steps(0, Nil)
  }
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
case class SimpleMatcher(pattern: Pattern) extends Matcher {
  def score(text: String): Match =
    // TODO each call to score should create it's own parser instance, so old cached results don't accumulate forever
    // One of the MemoParser instances I create below should fully parse the given text using all available strategies
    ???
}

object Parser {

  type Result[A]     = Writer[Steps, A]
  type Choices[A]    = Stream[Eval, A]
  type Results[A]    = Nested[Choices, Result, A]
  type PartParser[A] = StateT[Results, List[Char], A]

  type FullParser[A] = IndexedStateT[Results, List[Char], Nil.type, A]
  type BestParser[A] = IndexedStateT[Result, List[Char], Nil.type, A]
  type MemoParser[A] = ReaderT[BestParser, Caches, A]

  type AskCaches[F[_]]  = Ask[F, Caches]
  type StatefulText[F[_]] = Stateful[F, List[Char]]
  type TellSteps[F[_]]   = Tell[F, Steps]

  implicit def canParse[F[_]](
    implicit S: StatefulText[F],
             T: TellSteps[F],
             M: Monad[F],
             A: Alternative[F]
  ): CanParse[F] = StateTellCanParse()(S, T, M, A)

  def parseKleene[F[_]: CanParse](parseInner: F[Unit]): F[Unit] =
    allRepetitionsOf(progressive(parseInner))

  def progressive[F[_]: CanParse, A](parseInner: F[A]): F[A] =
    for {
      before <- peekText[F]
      res    <- parseInner
      after  <- peekText
      _      <- (before != after).guard
    } yield res

  def allRepetitionsOf[F[_]](parseInner: F[Unit])(implicit F: CanParse[F]): F[Unit] =
    F.pure(()) <+> (parseInner >> allRepetitionsOf(parseInner))
 
  def matchLit[F[_]: CanParse](l: Char): F[Unit] =
    for {
      c <- popChar[F]
      _ <- (c == l).guard
      _ <- record(Steps.matchChar(l))
    } yield ()

  def parsePattern[F[_]: CanParse](pattern: Pattern): F[Unit] = {
    val noSkipChar = pattern match {
      case Pattern.Any        => skipAny <+> matchAny
      case Pattern.Lit(c)     => skipLit(c) <+> matchLit(c)
      case Pattern.Group(p)   => parseGroup(parsePattern(p))
      case Pattern.Kleene(p)  => parseKleene(parsePattern(p))
      case Pattern.Concat(ps) => ps.toList.traverse_(parsePattern[F])
    }

    (skipChar >> parsePattern(pattern)) <+> noSkipChar
  }

  def parseGroup[F[_]: CanParse, A](parseInner: F[A]): F[A] =
    record[F](Steps.enter) *> parseInner <* record(Steps.leave)


  def matchAny[F[_]: CanParse]: F[Unit] =
    for {
      c <- popChar[F]
      _ <- record(Steps.matchChar(c))
    } yield ()

  def skipAny[F[_]: CanParse]: F[Unit] =
    record(Steps.skipAny)

  def skipLit[F[_]: CanParse](l: Char): F[Unit] =
    record(Steps.skipLit(l))

  def skipChar[F[_]: CanParse]: F[Unit] =
    for {
      c <- popChar[F]
      _ <- record(Steps.skipText(c))
    } yield ()

  def popChar[F[_]](implicit F: CanParse[F]): F[Char] =
    F.popChar

  def peekText[F[_]](implicit F: CanParse[F]): F[List[Char]] =
    F.peekText

  def record[F[_]](steps: Steps)(implicit F: CanParse[F]): F[Unit] =
    F.record(steps)

  def pure[F[_], A](a: A)(implicit F: CanParse[F]): F[A] =
    F.pure(a)

  // WARNING: the id String is a hack to do equality on functions.
  //
  // Given two calls memoise(id1)(f1) and memoise(id2)(f2)
  // We MUST ensure that if f1 != f2, then id1 != id2
  // We should also ensure that if f1 == f2, then id1 == id2, but this is less important
  //
  // This whole memoisation approach is a bit off, but should work nicely for this code.
  def memoise[F[_]: Functor, A, B](id: String)(f: A => B)(implicit F: AskCaches[F]): F[A => B] =
    F.ask.map(_.memoise(id, f))

  class Caches() {
    private val caches: HashMap[String, HashMap[_, _]] = new HashMap()

    def memoise[A, B](id: String, f: A => B): A => B = {
      val cache =
        Option(caches.get(id)) match {
          case Some(cache) =>
            cache
          case None =>
            val cache = new HashMap[A, B]()
            caches.put(id, cache)
            cache
        }

      val typedCache = cache.asInstanceOf[HashMap[A,B]]

      (a: A) =>
        Option(typedCache.get(a)) match {
          case Some(b) =>
            b
          case None =>
            val b = f(a)
            typedCache.put(a, b)
            b
        }
    }
  }
}

trait CanParse[F[_]] extends Monad[F] with Alternative[F] {
  def record(steps: Steps): F[Unit]
  def peekText: F[List[Char]]
  def popChar: F[Char]
}

case class StateTellCanParse[F[_]](implicit S: Parser.StatefulText[F], T: Parser.TellSteps[F], M: Monad[F], A: Alternative[F]) extends CanParse[F] {

  def record(steps: Steps): F[Unit] =
    T.tell(steps)

  def peekText: F[List[Char]] =
    S.get

  def popChar: F[Char] =
    S.get.flatMap {
      case char :: rest => S.set(rest) >> M.pure(char)
      case Nil          => A.empty[Char]
    }

   def pure[A](x: A): F[A]                                = M.pure(x)
   def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]        = M.flatMap(fa)(f)
   def tailRecM[A, B](a: A)(f: A => F[Either[A,B]]): F[B] = M.tailRecM(a)(f)
   def empty[A]: F[A]                                     = A.empty
   def combineK[A](x: F[A],y: F[A]): F[A]                 = A.combineK(x, y)
}

/*
object Parser {

  def score(pattern: Pattern, text: String): Match =
    parsePatternToEnd
      .gather(pattern, text.toList)
      .map(res => SimpleMatch(res.steps.reverse))
      .minBy(_.score)

  val parsePattern: Parser[Unit] =
    (parseConcat ++
    parseKleene ++
    parseGroup ++
    matchAny ++
    matchLit ++
    skipAny ++
    skipLit ++
    skipText.flatMap(_ => parsePattern)).memoised

  val parsePatternToEnd: Parser[Unit] =
    for {
      _    <- parsePattern
      left <- peekRemainingText
      _    <- traverse_(left)(c => record(SkipText(c)))
    } yield ()

  def withPattern[A](p: Pattern, matcher: Parser[A]): Parser[A] =
    Parser((_, text) => matcher.gather(p, text))

  def whenPatternIs[P](f: PartialFunction[Pattern, P]): Parser[P] =
    Parser {
      (pattern, text) =>
        if (f.isDefinedAt(pattern)) List(Result(Nil, text, f(pattern)))
        else                        Nil
    }
}

case class SimpleMatch(steps: List[Step]) extends Match {

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
}
*/
