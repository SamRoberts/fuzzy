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

case class Steps(score: Int, steps: List[Step]) extends Match {
  def matchedText: String =
    steps.collect { case Step.MatchChar(c) => c }.mkString
}

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

  val parser = FullParser.parsePattern(pattern)

  def score(text: String): Match = {
    val caches = new Caches()
    val res = parser.run(caches)(text.toList).steps
    caches.printState
    res
  }
}

// OK, so this is better than nothing, but we still take ages occassionally,
// I think probably when we generate a large enough pattern which is entirely
// surrounded by a group or kleene, at which point the intermediate caching
// in FullParser doesn't help at all.
//
// It really feels like we have to figure out how to make FullParser insert
// it's caching tendrils into nested patterns, maybe to the point of doing
// everything with FullParsers?
//
// I don't think this is going to look much like traditional parsers by the
// time I am finished, but maybe that was never going to work ...
//
// On the  bright side, all the tests passed first thing when I finished this
// code! Amazing how well that works!

case class FullParser(functionId: String, run: Caches => (List[Char] => FullParser.Result))

object FullParser {
  case class Result(steps: Steps)

  def parsePattern(pattern: Pattern): FullParser = pattern match {
    case Pattern.Concat(ps) => ps.foldRight(skipRemainingText)(prependPattern(_, _))
    case _                  => prependPattern(pattern, skipRemainingText)
  }

  def prependPattern(first: Pattern, last: FullParser): FullParser = {
    val firstP     = PartParser.parsePattern(first)
    val functionId = first.toString + ", " + last.functionId

    FullParser(functionId, caches => {
      caches.memoise(functionId) { text =>
        firstP.run(text) match {
          case Nil =>
            // TODO introduce another split between PartParser which can fail and PartParser which can't?
            throw new Exception("impossible")

          case res =>
            val lastRun = last.run(caches)
            val paths   = res.map(firstRun => firstRun.steps |+| lastRun(firstRun.rest).steps)
            val best    = paths.minBy(_.score)
            Result(best)
        }
      }
    })
  }

  def skipRemainingText: FullParser =
    atomic("SkipRemaining")(text => Result(text.foldMap(Steps.skipText)))

  def atomic[A](functionId: String)(f: List[Char] => Result): FullParser = {
    FullParser(functionId, _.memoise(functionId)(f))
  }
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

  def parsePattern(pattern: Pattern): PartParser[Unit] = {
    val noSkipChar = pattern match {
      case Pattern.Any        => skipAny <+> matchAny
      case Pattern.Lit(c)     => skipLit(c) <+> matchLit(c)
      case Pattern.Group(p)   => parseGroup(parsePattern(p))
      case Pattern.Kleene(p)  => parseKleene(parsePattern(p))
      case Pattern.Concat(ps) => ps.toList.traverse_(parsePattern)
    }

    (skipText >> parsePattern(pattern)) <+> noSkipChar
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

  def skipText: PartParser[Unit] =
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

// WARNING: the id String is a hack to do equality on functions.
//
// Given two calls memoise(id1)(f1) and memoise(id2)(f2)
// We MUST ensure that f1 != f2 implies id1 != id2
// We should also ensure that f1 == f2 implies id1 == id2, but this is less important
//
// This whole memoisation approach is a bit off, but should work nicely for this code.
class Caches() {
  private val caches: HashMap[String, HashMap[_, _]] = new HashMap()

  def memoise[A, B](id: String)(f: A => B): A => B = {
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

  def printState(): Unit = {
    import collection.JavaConverters._
    println("")
    println("-----------------------------------------------")
    caches.asScala.foreach { case (key, cache) => println(s"$key: ${cache.size}") }
  }
}
