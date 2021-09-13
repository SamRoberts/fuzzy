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

package fuzzy.pattern.regex

import fastparse._, NoWhitespace._
import fastparse.{parse => fastparse}

import fuzzy.api.Pattern

/**
  * A regex-inspired representation for patterns.
  *
  * This representation does not map 1:1 with the pattern constructors:
  * multiple syntaxes can result in the same pattern, and some patterns can't
  * be expressed using this representation.
  *
  * This representation is good for conveniently specifying patterns, but can't
  * reliably be used to print arbitrary patterns.
  */
object RegexPattern {

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
}
