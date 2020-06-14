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

/** A class which understands how to index into patterns and text and pattern vs. text tables. */
case class Table[T : scala.reflect.ClassTag](text: String, pattern: String, element: T) {

  val patternLength: Int = pattern.length + 1 // patternIx can be length of pattern, to represent consumption is complete
  val textLength: Int = text.length + 1 // textIx can be length of text, to represent consumption is complete

  val elems = Array.fill(textLength * patternLength)(element)

  def apply(textIx: Int, patternIx: Int): T =
     elems(tableIx(textIx, patternIx))

   def update(textIx: Int, patternIx: Int, elem: T): Unit =
     elems(tableIx(textIx, patternIx)) = elem

  def tableIx(textIx: Int, patternIx: Int): Int =
    textIx * patternLength + patternIx

  def textIx(tableIx: Int): Int =
    tableIx / patternLength

  def patternIx(tableIx: Int): Int =
    tableIx % patternLength
}

