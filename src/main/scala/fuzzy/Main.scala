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

import io.Source

object Main {

  // TODO use decline for CLI
  // TODO use os-lib or see what cats has for file IO, and properly close file too

  def main(args: Array[String]): Unit = {
    val patternFile = args(0)
    val textFile    = args(1)
    val pattern     = Source.fromFile(patternFile, "UTF-8").mkString
    val text        = Source.fromFile(textFile, "UTF-8").mkString

    println(Matcher(pattern).score(text).matchedText)
  }

}

