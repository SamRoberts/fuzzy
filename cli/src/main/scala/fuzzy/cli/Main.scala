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

package fuzzy.cli

import cats.data.Validated
import cats.effect.{ExitCode, IO}
import cats.implicits._
// TODO break implicit wildcard import up into the syntax and instances we need

import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp

import java.nio.file.{Files, Paths, Path}

import fuzzy.api.{Pattern, Matcher}
import fuzzy.matcher.loop.LoopMatcher

object Main extends CommandIOApp(
  name = "fuzzy",
  header = "Fuzzily match a pattern against some text",
  version = "0.3.0"
) {

  def mkMatcher(pattern: Pattern): Matcher =
    LoopMatcher(pattern)

  def pathOpt(long: String, help: String): Opts[Path] =
    Opts
      .option[String](
        long = long,
        help = help,
        metavar = "path"
      )
      .mapValidated(path =>
        Validated
          .catchNonFatal(Paths.get(path))
          .leftMap(_.getMessage)
          .toValidatedNel
      )

  val patternOpt = pathOpt("pattern", "Path to file containing pattern")
  val textOpt    = pathOpt("text", "Path to text file to match")

  def readPath(path: Path): IO[String] =
    IO {
      val bytes = Files.readAllBytes(path)

      // using platform's default charset seems like the right approach
      // when reading files on the platform
      new String(bytes)
    }

  override def main: Opts[IO[ExitCode]] =
    (patternOpt, textOpt).mapN {
      (patternPath, textPath) => for {

        pattern <- readPath(patternPath)
        pattern <- IO.fromEither(Pattern.parse(pattern).leftMap(new Exception(_)))
        text    <- readPath(textPath)
        matcher  = mkMatcher(pattern)
        result   = matcher.score(text)
        _       <- IO { println(result.matchedText) }
      } yield ExitCode.Success
    }
}

