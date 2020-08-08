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

import fuzzy.api.{Match, Matcher, Pattern}

case class Result[A](steps: List[Step], rest: List[Char], value: A)

case class SimpleMatcher(pattern: Pattern) extends Matcher {
  def score(text: String): Match =
    Parser.score(pattern, text)
}

case class Parser[A](gather: (Pattern, List[Char]) => List[Result[A]]) {
  def map[B](f: A => B): Parser[B] =
    flatMap(a => Parser.pure(f(a)))

  def flatMap[B](f: A => Parser[B]): Parser[B] =
    Parser(
      (pattern, text) =>
        for {
          resA <- gather(pattern, text)
          resB <- f(resA.value).gather(pattern, resA.rest)
        } yield Result(resB.steps ++ resA.steps, resB.rest, resB.value)
    )

  def filter(f: A => Boolean): Parser[A] =
    Parser(
      (pattern, text) =>
        gather(pattern, text).filter(resA => f(resA.value))
    )

  def withFilter(f: A => Boolean): Parser[A] =
    filter(f)

  def ++(other: Parser[A]): Parser[A] =
    Parser(
      (pattern, text) =>
        gather(pattern, text) ++ other.gather(pattern, text)
    )
}

object Parser {

  import Pattern._
  import Step._

  // TODO my absurdly inefficient test matcher implementation makes it unrealistic to do this test on reasonable sized inputs
  //      improve effiency of test matcher implementation

  def score(pattern: Pattern, text: String): Match =
    parsePatternToEnd
      .gather(pattern, text.toList)
      .map(res => SimpleMatch(res.steps.reverse))
      .minBy(_.score)

  def parsePatternToEnd: Parser[Unit] =
    for {
      _    <- parsePattern
      left <- peekRemainingText
      _    <- traverse_(left)(c => record(SkipText(c)))
    } yield ()

  def parsePattern: Parser[Unit] =
    parseConcat ++
    parseKleene ++
    parseGroup ++
    matchAny ++
    matchLit ++
    skipAny ++
    skipLit ++
    skipText.flatMap(_ => parsePattern)

  def parseConcat: Parser[Unit] =
    for {
      ps <- whenPatternIs { case Concat(ps) => ps }
      _  <- traverse_(ps) { p => withPattern(p, parsePattern) }
    } yield ()

  def parseKleene: Parser[Unit] =
    for {
      inner <- whenPatternIs { case Kleene(p) => p }
      _     <- withPattern(inner, parsePatternMany)
    } yield ()
 
 def parsePatternMany: Parser[Unit] =
    pure(()) ++ (for {
      before <- peekRemainingText 
      _      <- parsePattern
      after  <- peekRemainingText
      if (before != after)
      _      <- parsePatternMany
    } yield ())

  def parseGroup: Parser[Unit] =
    for {
      p <- whenPatternIs { case Group(p) => p }
      _ <- record(Enter)
      _ <- withPattern(p, parsePattern)
      _ <- record(Leave)
    } yield ()

  def matchAny: Parser[Unit] =
    for {
      _ <- whenPatternIs { case Any => () }
      c <- readText
      _ <- record(MatchChar(c))
    } yield ()

  def matchLit: Parser[Unit] =
    for {
      p <- whenPatternIs { case Lit(p) => p }
      c <- readText
      if (c == p)
      _ <- record(MatchChar(c))
    } yield ()

  def skipAny = skipParser { case Any => SkipAny }
  def skipLit = skipParser { case Lit(p) => SkipLit(p) }

  def skipText = readText.flatMap(c => record(SkipText(c)))

  def skipParser(skip: PartialFunction[Pattern, Step]): Parser[Unit] =
    whenPatternIs(skip).flatMap(record)

  def peekRemainingText: Parser[List[Char]] =
    Parser((_, text) => List(Result(Nil, text, text)))

  def readText: Parser[Char] =
    Parser(
      (pattern, text) => text match {
        case char :: rest => List(Result(Nil, rest, char))
        case Nil          => Nil
      }
    )

  def withPattern[A](p: Pattern, matcher: Parser[A]): Parser[A] =
    Parser((_, text) => matcher.gather(p, text))

  def whenPatternIs[P](f: PartialFunction[Pattern, P]): Parser[P] =
    Parser {
      (pattern, text) =>
        if (f.isDefinedAt(pattern)) List(Result(Nil, text, f(pattern)))
        else                        Nil
    }

  def record(step: Step): Parser[Unit] =
    Parser((pattern, text) => List(Result(List(step), text, ())))

  // TODO as with many of these methods, can be replaced with standard cats functionality prtty easily
  def traverse_[A](values: Seq[A])(f: A => Parser[Unit]): Parser[Unit] =
    values match {
      case Nil          => pure(())
      case head +: tail => f(head).flatMap(_ => traverse_(tail)(f))
    }

  def pure[A](value: A): Parser[A] =
    Parser((_, text) => List(Result(Nil, text, value)))
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

sealed trait Step

object Step {
  case class MatchChar(c: Char) extends Step
  case class SkipText(c: Char) extends Step
  case class SkipLit(c: Char) extends Step
  case object SkipAny extends Step
  case object Enter extends Step
  case object Leave extends Step
}

