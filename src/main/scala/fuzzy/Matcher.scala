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

case class Matcher(pattern: Pattern, trace: Boolean = false) {

  // TODO replace trace with proper log support

  import Matcher._

  // arrays are indexed by pix
  // given pattern "a.b(cde*)*fg.*" as an example, we create arrays with elements as follows,
  // with ., {*, *}, END representing wildcard, start and end of kleene group, and end of pattern, respectively.
  // [ a, ., b, {*, c, d, {*, e, *}, *}, f, g, {*, ., *}, END ]
  //
  // tables are indexed by pix and tix
  // given text the pattern above and text "azbcdeeecdfgz" as an example,
  // we create a table to hold a cell for every item in the cross-product of the pattern array above,
  // plus the text array [ a, z, b, c, d, e, e, e, c, d, f, g, z, END ]

  // this is probably the minimal set of arrays
  // might also want to create tables pre-computing skip penalties, or maybe cumulative skip penalty in kleene groups
  // but for now we'll just compute these arrays and see how it goes

  val cases    = patternArray[Int] (ANY,  _ => LIT,       _ => OPEN,    _ => CLOSE,   END).toArray
  val literals = patternArray[Char]('?',  identity[Char], _ => '?',     _ => '?',     '?').toArray
  val backs    = patternArray[Int] (-1,   _ => -1,        _ => -1,      _.length + 1, -1).toArray
  val skips    = patternArray[Int] (-1,   _ => -1,        _.length + 2, _ => -1,      -1).toArray

  val maxKleeneDepth = pattern.fold[Int](0, _ => 0, identity[Int], _ + 1, xs => if (xs.isEmpty) 0 else xs.max)

  def score(text: String): Match = {

    // scores(tix)(pix) holds smallest penalty hit to match remaining text and pattern, starting at this index of each
    // pnexts(tix)(pix) holds pix for tix+1 position which corresponds to one of the optimal paths
    // matchs(tix)(pix) holds boolean flag telling us whether path to next text level involved matching text or not
    //
    // control flow state: indices into pattern and text. start at end and work our way back
    //                     also variables for storing best next step
    //
    // temp state: temporary variables for storing the best attempted path from a particular state
    //
    // scoreBack state: keeps track of scores at different levels when traversing nested kleene groups

    val scores = Array.ofDim[Int](text.length + 1, cases.length)
    val pnexts = Array.ofDim[Int](text.length + 1, cases.length)
    val matchs = Array.ofDim[Boolean](text.length + 1, cases.length)

    var pix = cases.length - 1
    var tix = text.length

    var tempScore = Int.MaxValue
    var tempPNext = -1
    var tempMatch = false

    val scoreBack = Array.fill(maxKleeneDepth + 1)(0)

    def logState(): Unit = {
      def showi(i: Int): String  = if (i == -1) " " else i.toString
      def showc(c: Char): String = c.toString

      println("==== state ====")
      println("")
      print("cases:    ")
      println(cases.map(showi).mkString(", "))
      print("literals: ")
      println(literals.map(showc).mkString(", "))
      print("backs:    ")
      println(backs.map(showi).mkString(", "))
      print("skips:    ")
      println(skips.map(showi).mkString(", "))
      println("")
      println("scores:")
      println(scores.map(_.map(showi).mkString(", ")).mkString("\n"))
      println("")
      println("pnexts:")
      println(pnexts.map(_.map(showi).mkString(", ")).mkString("\n"))
      println("")
    }

    // attempt and attemptApply use temp variables and pix and tix to try different paths from current state

    def attemptSkipText(): Unit = {
      if (trace) print(s"  atttempt to skip text: ")
      attemptGen(scores(tix + 1)(pix) + 1, pix, false)
    }

    def attemptMatch(): Unit = {
      if (trace) print(s"  atttempt to match text: ")
      attemptGen(scores(tix + 1)(pix + 1), pix + 1, true)
    }

    def attemptJumpForward(pixDiff: Int, skipPenalty: Int): Unit = {
      if (trace) print(s"  atttempt jump forward $pixDiff with penalty $skipPenalty and do optimal step from there: ")
      attemptGen(
        scores(tix)(pix + pixDiff) + skipPenalty,
        pnexts(tix)(pix + pixDiff),
        matchs(tix)(pix + pixDiff)
      )
    }

    def attemptJumpBackSkipText(pixDiff: Int, skipPenalty: Int): Unit = {
      if (trace) print(s"  atttempt jump back $pixDiff with penalty $skipPenalty and skip text: ")
      attemptGen(
        scores(tix + 1)(pix - pixDiff) + skipPenalty + 1,
        pix - pixDiff,
        false
      )
    }

    def attemptJumpBackMatchText(pixDiff: Int, skipPenalty: Int): Unit = {
      if (trace) print(s"  atttempt jump back $pixDiff with penalty $skipPenalty and match text: ")
      attemptGen(
        scores(tix + 1)(pix - pixDiff + 1) + skipPenalty,
        pix - pixDiff + 1,
        true
      )
    }

    def attemptGen(newScore: Int, newPNext: Int, newMatch: Boolean): Unit =
      if (newScore < tempScore) {
        if (trace) println(s"OK, new score: $newScore, new pnext: $newPNext, new match: $newMatch")
        tempScore = newScore
        tempPNext = newPNext
        tempMatch = newMatch
      } else {
        if (trace) println(s"NO, new score $newScore not good enough")
      }

    def applyAttempt(): Unit = {
      if (trace) println("")
      scores(tix)(pix) = tempScore
      pnexts(tix)(pix) = tempPNext
      matchs(tix)(pix) = tempMatch
      tempScore        = Int.MaxValue
      tempPNext        = -1
    }

    // first, populate last row with scores for skipping remaining pattern once we get to end of string
    // remember we can skip any kleene expressions we encounter

    while (pix >= 0) {
      if (trace) println(s"processing text ix: $tix, pattern ix: $pix")
      cases(pix) match {
        case END =>
          tempScore = 0
          tempPNext = -1
          tempMatch = false

        case ANY | LIT =>
          attemptJumpForward(1, 1)

        case CLOSE =>
          attemptJumpForward(1, 0)

        case OPEN =>
          attemptJumpForward(skips(pix), 0)
      }

      applyAttempt()
      pix -= 1
    }

    if (trace) logState()

    tix -= 1

    // now, go back through text 1 character at a time, filling out scores and pnexts as we go

    while (tix >= 0) {
      pix = cases.length - 1

      while (pix >= 0) {
        if (trace) println(s"processing text ix: $tix, pattern ix: $pix")

        cases(pix) match {
          case END =>
            attemptSkipText()

          case ANY =>
            attemptMatch()
            attemptSkipText()
            attemptJumpForward(1, 1)

          case LIT if literals(pix) == text(tix) =>
            attemptMatch()
            attemptJumpForward(1, 1)
            attemptSkipText()

          case LIT /*literal does not match*/ =>
            attemptJumpForward(1, 1)
            attemptSkipText()

          case OPEN =>
            attemptJumpForward(skips(pix), 0)
            attemptJumpForward(1, 0)
            attemptSkipText()

          case CLOSE =>
            // this is where the bulk of the kleene magic happens
            // we're going to check cost of skipping close token
            // and then we are going to check cost of skipping back to open ...
            // but wait! we haven't calculated that cost yet as it's in front of us in row!
            // so for CLOSE only we will check each possible skip from open to current pix
            // remembering that when we get back out of an inner kleene we can imagine we skipped entire thing

            attemptSkipText()
            attemptJumpForward(1, 0)

            var pixBack      = backs(pix)
            var scoreBackIdx = 0

            while (pixBack > 0) {
              cases(pix-pixBack) match {
                case END =>
                  throw new IllegalStateException(s"programmer error: encountered unexpected END symbol in '$this' case array")

                case ANY =>
                  attemptJumpBackMatchText(pixBack, scoreBack(scoreBackIdx))
                  attemptJumpBackSkipText(pixBack, scoreBack(scoreBackIdx))
                  scoreBack(scoreBackIdx) += 1

                case LIT if literals(pix-pixBack) == text(tix) =>
                  attemptJumpBackMatchText(pixBack, scoreBack(scoreBackIdx))
                  attemptJumpBackSkipText(pixBack, scoreBack(scoreBackIdx))
                  scoreBack(scoreBackIdx) += 1

                case LIT /*literal does not match*/ =>
                  attemptJumpBackSkipText(pixBack, scoreBack(scoreBackIdx))
                  scoreBack(scoreBackIdx) += 1

                case OPEN =>
                  attemptJumpBackSkipText(pixBack, scoreBack(scoreBackIdx))
                  scoreBack(scoreBackIdx+1) = scoreBack(scoreBackIdx)
                  scoreBackIdx += 1

                case CLOSE =>
                  attemptJumpBackSkipText(pixBack, scoreBack(scoreBackIdx))
                  scoreBackIdx -= 1
              }

              pixBack  -= 1
            }
        }

        applyAttempt()
        pix -= 1
      }

      if (trace) logState()

      tix -= 1
    }

    // at this point, we should have filled out scores, pnexts, and matchs
    // optimal score for entire match is stored at scores(0)(0)
    // we are now traversing pnexts and matchs, building up matched text

    tix = 0
    pix = 0
    val matchedTextBuilder = new StringBuilder(text.length)

    while (tix < text.length) {
      if (matchs(tix)(pix)) { matchedTextBuilder += text(tix) }
      pix = pnexts(tix)(pix)
      tix += 1
    }

    MatchedMatch(matchedTextBuilder.toString, scores(0)(0))
  }

  def patternArray[T](
    anyT: T,
    litT: Char => T,
    openT: Seq[T] => T,
    closeT: Seq[T] => T,
    end: T
  ): Seq[T] = {
    val folded = pattern.fold[Seq[T]](
      Seq(anyT),
      c => Seq(litT(c)),
      identity[Seq[T]],
      innerTs => openT(innerTs) +: innerTs :+ closeT(innerTs),
      _.flatten
    )

    folded :+ end
  }
}

object Matcher {
  // enum for types of pattern element
  val ANY = 1
  val LIT = 2
  val OPEN = 3
  val CLOSE = 4
  val END = 5
}


case class MatchedMatch(matchedText: String, score: Int) extends Match
