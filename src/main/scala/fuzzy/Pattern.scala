package fuzzy

case class Pattern(pattern: String, trace: Boolean = false) {

  def withTrace: Pattern = copy(trace = true)

  val flow = ControlFlow(pattern)

  def score(text: String): Match = {
    var stepCount = 0

    // if tableFin(i,j) then tableVal(i,j) is final score from that state to end
    // tableAcc(i,j) is minimum accumulated penalty incurred from start to state
    // tableRec(i,j) records the next state the optimal path took
    val tableVal = Table[Int](text, pattern, -1)
    val tableAcc = Table[Int](text, pattern, -1)
    val tableRec = Table[Int](text, pattern, -1)

    def inner(parentStep: Int, textIx: Int, patternIx: Int, acc: Int): Int = {
      stepCount += 1
      val step = stepCount

      printTraceMsg("enter", step, "from", parentStep, text, textIx, pattern, patternIx)

      def set(score: Int) = score >= 0
      def patternFinished = patternIx >= pattern.length
      def textFinished    = textIx >= text.length
      def canFork         = set(forkIx)
      def forkIx          = flow.forks(patternIx)
      def mustGoto        = set(gotoIx)
      def gotoIx          = flow.gotos(patternIx)
      def nextIx          = patternIx+1
      def nextText        = textIx+1
      def patternControl  = { val c = pattern(patternIx); c == '*' || c == '(' || c == ')' }
      def isMatch         = { val c = pattern(patternIx); c == '.' || c == text(textIx) }


      if (set(tableVal(textIx, patternIx))) {
        val result = tableVal(textIx, patternIx)
        printTraceMsg("leave", step, "for", parentStep, text, textIx, pattern, patternIx, "cache", result)
        return result
      }

      if (set(tableAcc(textIx, patternIx)) && tableAcc(textIx, patternIx) <= acc) {
        // if we've already reached this state and our accumulated score isn't any better, we might as well give up now
        printTraceMsg("leave", step, "for", parentStep, text, textIx, pattern, patternIx, "throw", -1)
        return -1
      } else {
        tableAcc(textIx, patternIx) = acc
      }

      // ok, my attempt to not allocate heap in middle of loop is reaching epically stupid proportions ...
      var result        = -1
      var resTextIx     = -1
      var resTemplateIx = -1

      def attempt(newTextIx: Int, newTemplateIx: Int, penalty: Int): Unit = {
        val rawScore = inner(step, newTextIx, newTemplateIx, acc+penalty)
        val score    = if (set(rawScore)) rawScore+penalty else rawScore

        if (set(score) && (!set(result) || score < result)) {
          result        = score
          resTextIx     = newTextIx
          resTemplateIx = newTemplateIx
        }
      }

      if (patternFinished && textFinished) {
        result = 0
      }
      else if (patternFinished && !textFinished) {
        attempt(nextText, patternIx, 1)
      }
      else if (mustGoto) {
        attempt(textIx, gotoIx, 0)
      }
      else {
        if (canFork) {
           attempt(textIx, forkIx, 0)
        }

        if (patternControl) {
          attempt(textIx, nextIx, 0)
        }
        else if (textFinished) {
          attempt(textIx, nextIx, 1)
        }
        else if (isMatch) {
          attempt(nextText, nextIx, 0)
        }
        else {
          attempt(textIx, nextIx, 1)
          attempt(nextText, patternIx, 1)
        }
      }

      if (set(result)) {
        tableVal(textIx, patternIx) = result
      }
      if (set(resTextIx) && set(resTemplateIx)) {
        tableRec(textIx, patternIx) = tableRec.tableIx(resTextIx, resTemplateIx)
      }

      printTraceMsg("leave", step, "for", parentStep, text, textIx, pattern, patternIx, "score", result)

      result
    }

    val score = inner(0, 0, 0, 0)

    Match(text, this, score, tableRec)
  }

  def printTraceMsg(action: String, step: Int, connector: String, lastStep: Int, text: String, textIx: Int, pattern: String, patternIx: Int): Unit  = {
    if (trace) {
      val splitText    = Util.indexed(text, textIx)
      val splitPattern = Util.indexed(pattern, patternIx)
      print(Color.std)
      println(f"$action $step%3d $connector%4s $lastStep%3d: position: $splitText%12s pattern: $splitPattern%8s")
    }
  }

  def printTraceMsg(action: String, step: Int, connector: String, lastStep: Int, text: String, textIx: Int, pattern: String, patternIx: Int, scoreType: String, score: Int): Unit  = {
    if (trace) {
      val splitText    = Util.indexed(text, textIx)
      val splitPattern = Util.indexed(pattern, patternIx)
      print(Color.leaveText)
      println(f"$action $step%3d $connector%4s $lastStep%3d: position: $splitText%12s pattern: $splitPattern%8s, $scoreType $score%2d")
    }
  }
}

