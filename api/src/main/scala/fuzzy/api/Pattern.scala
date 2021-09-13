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

  case object Any extends Pattern
  case class Lit(char: Char) extends Pattern
  case class Group(inside: Pattern) extends Pattern
  case class Kleene(inside: Pattern) extends Pattern
  case class Concat(elements: Seq[Pattern]) extends Pattern
}
