object Main {

  def main(args: Array[String]): Unit = {
    val cases = List(
      ("", "", 0),
      ("", "aabb", 4),
      ("aa", "", 2),
      ("a*", "", 0),
      (".*", "aabb", 0),
      ("aa", "aba", 1),
      ("a.a", "aba", 0),
      ("aa.", "aba", 2),
      ("aa.*bb", "afaffbb", 1),
      ("za*bb", "zbb", 0),
      ("a..*bb", "afaffbb", 0),
      ("z(ac)*z", "zacdacacz", 1),
      ("z(a*c)*z", "zacdaaacaacz", 1)
    )

    val results = cases.map { case (template, text, expected) => (template,  text, expected, Template(template).score(text)) }

    results.filter { case (_, _, expected, actual) => expected != actual }.foreach {
      case (template, text, expected, actual) =>
        Template(template).withTrace.score(text)
        println(s"final score $actual does not equal $expected")
        println()
    }

    println(" template | text         | expected | actual ")
    println("----------|--------------|----------|--------")
    println(
      results.map { case (template, text, expected, actual) =>
        val line = f" $template%8s | $text%12s | $expected%8d | $actual%6d "
        if (expected == actual) line else Console.RED + line + Console.RESET
      }.mkString("\n")
    )
  }
}

case class Template(template: String, trace: Boolean = false) {

  val forks      = createForks(template)
  val traceForks = forks.length > 0 && forks.forall(_ < 10)

  def withTrace: Template = copy(trace = true)

  // TODO add regex like repeating patterns rather that just wildcards
  //      do this sooner rather than later in case it stuffs up logic
  // TODO implement matching. Could be higher priority, but how to implement matching?
  //      Can we record at each table index what the index of the next step we took was?
  //      Then how to recover match from that?
  //      Seems to me this is where Template class with pattern <=> tableIx comes in handy
  //      But want proper repeating patterns first in case it stuffs up table logic
  //      So do repeating patterns first, then template/templateIx change, then matching
  // TODO get benchmarks up and running and then start investigating whether we can make
  //      code more optimised and safer. Current approach is both overly simplistic but also
  //      more skeptical of creating new objects than standard Scala code.

  def score(text: String): Int = {
    var stepCount = 0

    // if tableFin(i,j) then tableVal(i,j) is final score
    // tableAcc(i,j) is minimum accumulated penalty incurred to i,j
    val tableVal = Table[Int](text, template, -1)
    val tableAcc = Table[Int](text, template, -1)

    def inner(parentStep: Int, textIx: Int, templateIx: Int, acc: Int): Int = {
      stepCount += 1
      val step = stepCount

      printTraceMsg("enter", step, "from", parentStep, text, textIx, template, templateIx)

      val templateFinished = templateIx >= template.length
      val textFinished     = textIx >= text.length
      def canFork          = set(forks(templateIx))
      def bigForkIx        = (templateIx+1) max forks(templateIx)
      def smallForkIx      = (templateIx+1) min forks(templateIx)
      def templateControl  = { val c = template(templateIx); c == '*' || c == '(' || c == ')' }
      def isMatch          = { val c = template(templateIx); c == '.' || c == text(textIx) }


      if (set(tableVal(textIx, templateIx))) {
        val result = tableVal(textIx, templateIx)
        printTraceMsg("leave", step, "for", parentStep, text, textIx, template, templateIx, "cache", result)
        return result
      }

      if (set(tableAcc(textIx, templateIx)) && tableAcc(textIx, templateIx) <= acc) {
        // if we've already reached this state and our accumulated score isn't any better, we might as well give up now
        printTraceMsg("leave", step, "for", parentStep, text, textIx, template, templateIx, "throw", -1)
        return -1
      } else {
        tableAcc(textIx, templateIx) = acc
      }

      val result =
        if (templateFinished) {
          // correct  whether textIx == or < than text.length
          text.length - textIx
        }
        else if (!templateFinished && templateControl && !canFork) {
          inner(step, textIx, templateIx+1, acc)
        }
        else if (!templateFinished && templateControl && canFork) {
          minSet(inner(step, textIx, bigForkIx, acc), inner(step, textIx, smallForkIx, acc))
        }
        else if (!templateFinished && !templateControl && textFinished && !canFork) {
          1 + inner(step, textIx, templateIx+1, acc+1)
        }
        else if (!templateFinished && !templateControl && textFinished && canFork) {
          incSet(minSet(inner(step, textIx, bigForkIx, acc+1), inner(step, textIx, smallForkIx, acc+1)))
        }
        else if (!templateFinished && !templateControl && !textFinished && isMatch && !canFork) {
          inner(step, textIx+1, templateIx+1, acc)
        }
        else if (!templateFinished && !templateControl && !textFinished && isMatch && canFork) {
          minSet(inner(step, textIx+1, bigForkIx, acc), inner(step, textIx+1, smallForkIx, acc))
        }
        else if (!templateFinished && !templateControl && !textFinished && !isMatch && !canFork) {
          incSet(minSet(inner(step, textIx, templateIx+1, acc+1), inner(step, textIx+1, templateIx, acc+1)))
        }
        else if (!templateFinished && !templateControl && !textFinished && !isMatch && canFork) {
          incSet(minSet(inner(step, textIx, bigForkIx, acc+1), inner(step, textIx, smallForkIx, acc+1), inner(step, textIx+1, templateIx, acc+1)))
        }
        else {
          throw new Exception("programmer error: unexpected case")
        }

      if (set(result)) tableVal(textIx, templateIx) = result

      printTraceMsg("leave", step, "for", parentStep, text, textIx, template, templateIx, "score", result)

      result
    }

    inner(0, 0, 0, 0)
  }

  // we use -1 to represent unset int scores and similar numbers
  // ok, using -1 as unset score has lead to some very ugly code below to work with these numbers ...

  def set(score: Int)  = score >= 0

  def minSet(score1: Int, score2: Int): Int =
    if (set(score1) && set(score2)) score1 min score2
    else                            score1 max score2 // only works as unset numbers are negative

  def minSet(score1: Int, score2: Int, score3: Int): Int =
    minSet(minSet(score1, score2), minSet(score1, score3))

  def incSet(score: Int): Int =
    if (set(score)) score+1 else score

  /** Returns an array of possible forks for each position, -1 if no fork */
  def createForks(template: String): Array[Int] = {
    val forks = Array.fill(template.length)(-1)
    template.zipWithIndex.foldLeft(List.empty[Int]) { (scopes: List[Int], elem: (Char, Int)) =>
      val (c, ix) = elem
      (c, ix, ix+1 < template.length &&  template(ix+1) == '*', scopes) match {
        case ('(', ix, true,  _) =>
          throw new Exception(s"illegal (* pattern at ${Util.indexed(template, ix)}")

        case ('(', ix, false, _) =>
          ix +: scopes

        case (')', ix, _, Nil) =>
          throw new Exception(s"illegal ) at top level at ${Util.indexed(template, ix)}")

        case (')', ix, true, scopeStart :: lowerScopes) =>
          forks(scopes.head) = ix+1
          forks(ix)          = scopeStart
          lowerScopes

        case (')', _, false, _ :: lowerScopes) =>
          lowerScopes

        case ('*', 0, _, _) =>
          throw new Exception(s"illegal * at ${Util.indexed(template, 0)}")

        case ('*', ix, true, _) =>
          throw new Exception(s"illegal ** pattern at ${Util.indexed(template, ix)}")

        case ('*', _, false, _) =>
          scopes

        case (c, ix, true, _) =>
          forks(ix) = ix
          scopes

        case (c, _, _, _) =>
          scopes
      }
    }

    forks
  }

  def printTraceMsg(action: String, step: Int, connector: String, lastStep: Int, text: String, textIx: Int, template: String, templateIx: Int): Unit  = {
    if (trace) {
      val splitText = Util.indexed(text, textIx)
      val splitTemplate = Util.indexed(template, templateIx)
      if (traceForks) {
        val splitForks = Util.indexed(forks.map(i => if (set(i)) i.toString else ".").mkString, templateIx)
        println(f"$action $step%2d $connector%4s $lastStep%2d: position: $splitText%8s pattern: $splitTemplate%8s forks: $splitForks")
      } else {
        println(f"$action $step%2d $connector%4s $lastStep%2d: position: $splitText%8s pattern: $splitTemplate%8s")
      }
    }
  }

  def printTraceMsg(action: String, step: Int, connector: String, lastStep: Int, text: String, textIx: Int, template: String, templateIx: Int, scoreType: String, score: Int): Unit  = {
    if (trace) {
      val splitText = Util.indexed(text, textIx)
      val splitTemplate = Util.indexed(template, templateIx)
      if (traceForks) {
        val splitForks = Util.indexed(forks.map(i => if (set(i)) i.toString else ".").mkString, templateIx)
        println(f"$action $step%2d $connector%4s $lastStep%2d: position: $splitText%8s pattern: $splitTemplate%8s forks: $splitForks, $scoreType $score%2d")
      } else {
        println(f"$action $step%2d $connector%4s $lastStep%2d: position: $splitText%8s pattern: $splitTemplate%8s, $scoreType $score%2d")
      }
    }
  }
}

/** A class which understands how to index into templates and text and template vs. text tables. */
case class Table[T : scala.reflect.ClassTag](text: String, template: String, element: T) {

  val templateLength: Int = template.length + 1 // templateIx can be length of template, to represent consumption is complete
  val textLength: Int = text.length + 1 // textIx can be length of text, to represent consumption is complete

  val elems = Array.fill(textLength * templateLength)(element)

  def apply(textIx: Int, templateIx: Int): T =
     elems(tableIx(textIx, templateIx))

   def update(textIx: Int, templateIx: Int, elem: T): Unit =
     elems(tableIx(textIx, templateIx)) = elem

  def tableIx(textIx: Int, templateIx: Int): Int =
    textIx * templateLength + templateIx
}

object Util {
  def indexed(text: String, ix: Int): String = {
    val spaced = text + " "
    spaced.substring(0, ix) + Console.BLUE_B + spaced(ix) + Console.RESET + spaced.substring(ix+1)
  }
}
