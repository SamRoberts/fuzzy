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

// a dumb implementation of pattern to start with, will consider how to improve while implementing new predicatable algorithm

sealed trait Pattern

object Pattern {
  case object Any extends Pattern
  case class Lit(char: Char) extends Pattern
  case class Group(inside: Pattern) extends Pattern
  case class Kleene(inside: Pattern) extends Pattern
  case class Concat(elements: Array[Pattern]) extends Pattern

  implicit val factory = new PatternFactory[Pattern] {
    type Builder = Pattern

    def freeze(builder: Builder): Pattern = builder

    def any(text: String): Builder =
      Any

    def lit(char: Char, text: String): Builder =
      Lit(char)

    def group(inside: Builder, textPre: String, textPost: String): Builder =
      Group(inside)

    def kleene(inside: Builder, testPre: String, textPost: String): Builder =
      Kleene(inside)

    def concat(builders: Seq[Builder]): Builder =
      Concat(builders.toArray)
  }
}
