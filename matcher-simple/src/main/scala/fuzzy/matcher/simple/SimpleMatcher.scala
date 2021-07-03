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

import annotation.tailrec

import cats.{Alternative, Applicative, Functor, Monad, Monoid}
import cats.data.{IndexedStateT, Nested, ReaderT, StateT, Writer}
import cats.mtl.{Ask, Stateful, Tell}
import cats.instances._
import cats.implicits._

import fs2.{Pure, Stream}

import cats.instances.function._

import fuzzy.api.{Match, Matcher, Pattern}

import java.util.HashMap

case class Steps(score: Int, steps: List[Step])

object Steps {
  def matchChar(c: Char) = Steps(0, List(Step.MatchChar(c)))
  def skipText(c: Char)  = Steps(1, List(Step.SkipText(c)))
  def skipLit(c: Char)   = Steps(1, List(Step.SkipLit(c)))
  val skipAny            = Steps(1, List(Step.SkipAny))
  val enter              = Steps(0, List(Step.Enter))
  val leave              = Steps(0, List(Step.Leave))
  val empty              = Steps(0, Nil)

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

case class PartParser[A](run: List[Char] => List[PartParser.Result[A]])

object PartParser {
  case class Result[A](rest: List[Char], steps: Steps, value: A)

  implicit val instances = new Monad[PartParser] with Alternative[PartParser] {
    def empty[A] = PartParser(text => Nil)

    def combineK[A](x: PartParser[A], y: PartParser[A]) =
      PartParser(text =>
        x.run(text) ++ y.run(text)
      )

    def pure[A](a: A) = PartParser(text => List(Result(text, Monoid[Steps].empty, a))) 

    def flatMap[A, B](fa: PartParser[A])(f: A => PartParser[B]) =
      PartParser(text =>
        for {
          resA <- fa.run(text)
          resB <- f(resA.value).run(resA.rest)
        } yield Result(resB.rest, resA.steps |+| resB.steps, resB.value)
      )

  def tailRecM[A, B](a: A)(f: A => PartParser[Either[A, B]]): PartParser[B] =
    PartParser(text => {
      @tailrec
      def inner(acc: List[Result[B]], resAs: List[Result[A]]): List[Result[B]] = {
        val (newResAs, newAcc) = resAs
                                   .flatMap(resA => f(resA.value).run(resA.rest))
                                   .map {
                                     case Result(steps, rest, Left(a))  => Left(Result(steps, rest, a))
                                     case Result(steps, rest, Right(b)) => Right(Result(steps, rest, b))
                                   }
                                   .separate
        inner(newAcc ++ acc, newResAs)
      }
      inner(Nil, List(Result(text, Monoid[Steps].empty, a)))
    })
  }
}

case class FullParser[A](run: (Caches, List[Char]) => FullParser.Result[A])

object FullParser {
  case class Result[A](steps: Steps, value: A)
}

// WARNING: the id String is a hack to do equality on functions.
//
// Given two calls memoise(id1)(f1) and memoise(id2)(f2)
// We MUST ensure that if f1 != f2, then id1 != id2
// We should also ensure that if f1 == f2, then id1 == id2, but this is less important
//
// This whole memoisation approach is a bit off, but should work nicely for this code.
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

object Parser {

  def parsePattern(pattern: Pattern): PartParser[Unit] = {
    val noSkipChar = pattern match {
      case Pattern.Any        => skipAny <+> matchAny
      case Pattern.Lit(c)     => skipLit(c) <+> matchLit(c)
      case Pattern.Group(p)   => parseGroup(parsePattern(p))
      case Pattern.Kleene(p)  => parseKleene(parsePattern(p))
      case Pattern.Concat(ps) => ps.toList.traverse_(parsePattern)
    }

    (skipChar >> parsePattern(pattern)) <+> noSkipChar
  }

  def parseKleene(parseInner: PartParser[Unit]): PartParser[Unit] =
    allRepetitionsOf(progressive(parseInner))

  def progressive[A](parseInner: PartParser[A]): PartParser[A] =
    for {
      before <- peekText
      res    <- parseInner
      after  <- peekText
      _      <- (before != after).guard[PartParser]
    } yield res

  def allRepetitionsOf(parseInner: PartParser[Unit]): PartParser[Unit] =
    pure(()) <+> (parseInner >> allRepetitionsOf(parseInner))
 
  def parseGroup[A](parseInner: PartParser[A]): PartParser[A] =
    record(Steps.enter) *> parseInner <* record(Steps.leave)

  def matchLit(l: Char): PartParser[Unit] =
    for {
      c <- popChar
      _ <- (c == l).guard[PartParser]
      _ <- record(Steps.matchChar(l))
    } yield ()

  def matchAny: PartParser[Unit] =
    for {
      c <- popChar
      _ <- record(Steps.matchChar(c))
    } yield ()

  def skipAny: PartParser[Unit] =
    record(Steps.skipAny)

  def skipLit(l: Char): PartParser[Unit] =
    record(Steps.skipLit(l))

  def skipChar: PartParser[Unit] =
    for {
      c <- popChar
      _ <- record(Steps.skipText(c))
    } yield ()

  def popChar: PartParser[Char] =
    PartParser {
      case Nil         => Nil
      case chr :: rest => List(PartParser.Result(rest, Steps.empty, chr))
    }

  def peekText: PartParser[List[Char]] =
    PartParser(text => List(PartParser.Result(text, Steps.empty, text)))

  def record(steps: Steps): PartParser[Unit] =
    PartParser(text => List(PartParser.Result(text, steps, ())))

  def pure[A](a: A): PartParser[A] =
    PartParser(text => List(PartParser.Result(text, Steps.empty, a)))
}

