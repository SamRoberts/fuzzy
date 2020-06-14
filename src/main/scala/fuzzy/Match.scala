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

case class Match(text: String, pattern: Matcher, score: Int, tableRec: Table[Int]) {

  def matchedText: String =
    trace().sliding(2).collect {
      case List((textIxNow, patternIxNow), (textIxNext, patternIxNext)) if (textIxNext == textIxNow+1) && (patternIxNext == patternIxNow+1) && textIxNow < text.length =>
        text(textIxNow)
    }.mkString

  def tracePretty: String = {
    def err(str: String) = Color.errBack + str + Color.stdBack

    val traceChars = trace().sliding(2).map {
      case List((textIxNow, patternIxNow), (textIxNext, patternIxNext)) =>
        if ((textIxNext != textIxNow) && (patternIxNext != patternIxNow)) {
          text(textIxNow).toString -> pattern.pattern(patternIxNow).toString
        }
        else if ((textIxNext == textIxNow) && (patternIxNext != patternIxNow) && "()*".contains(pattern.pattern(patternIxNow))) {
          " " -> pattern.pattern(patternIxNow).toString
        }
        else if ((textIxNext == textIxNow) && (patternIxNext != patternIxNow)) {
          err(" ") -> pattern.pattern(patternIxNow).toString
        }
        else if ((textIxNext != textIxNow) && (patternIxNext == patternIxNow)) {
          text(textIxNow).toString -> err(" ")
        }
        else {
          err("?") -> err("?")
        }
    }.toVector

    traceChars.map(_._1).mkString + "\n" + traceChars.map(_._2).mkString
  }

  def trace(textIx: Int = 0, patternIx: Int = 0): List[(Int, Int)] = {
    // TODO put set somewhere accessible, or use scala's value types to represent index
    if (textIx >= 0 && patternIx >= 0) {
      val nextTableIx = tableRec(textIx, patternIx)
      (textIx -> patternIx) :: trace(tableRec.textIx(nextTableIx), tableRec.patternIx(nextTableIx))
    } else {
      Nil
    }
  }
}

