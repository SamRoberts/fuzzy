// TODO turn into tool which can write matched text and/or report score, and turn
//      current main method into proper tests. Attempt Scala hedgehog tests only?

// TODO get benchmarks up and running and then start investigating whether we can make
//      code more optimised and safer. Current approach is both overly simplistic but also
//      more skeptical of creating new objects than standard Scala code.

// TODO Use a logical representation of template rather than raw template. This will make it
//      easier to do other changes. Probably not realistic to do escaping before this.

// TODO support escaping within template pattern

// TODO Replace unstructured fork and jump instructions into scope instructions. To avoid
//      object allocations (but will have benchmarks to judge if I really care), can parse
//      template ahead of time to find out max env depth and pre-allocate env arrays.

// TODO once scope instructions are done, consider additional outputs, like printing
//      text which matched scope, similar to regex captures. Consider named scopes.

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
      ("z(ac)*z", "zz", 0),
      ("z(ac)*z", "zacaacacz", 1),
      ("z(a*c)*z", "zacaacacz", 0),
      ("z(a*c)*z", "zacdaaacaacz", 1),
      (".*b.*c", "zzzzzbxxxxc", 0),
      (".*b.*c", "yyyy", 2)
    )

    val results = cases.map { case (template, text, expected) => (template,  text, expected, Template(template).score(text).score) }

    results.filter { case (_, _, expected, actual) => expected != actual }.foreach {
      case (template, text, expected, actual) =>
        Template(template).withTrace.score(text)
        print(Color.std)
        println(s"final score $actual does not equal $expected")
        println()
    }

    print(Color.std)
    println(" template | text         | expected | actual ")
    println("----------|--------------|----------|--------")
    println(
      results.map { case (template, text, expected, actual) =>
        val color = if (expected == actual) Color.stdText else Color.errText
        f"$color $template%8s | $text%12s | $expected%8d | $actual%6d "
      }.mkString("\n")
    )
  }
}

case class Template(template: String, trace: Boolean = false) {

  def withTrace: Template = copy(trace = true)

  val flow = ControlFlow(template)

  def score(text: String): Match = {
    var stepCount = 0

    // if tableFin(i,j) then tableVal(i,j) is final score from that state to end
    // tableAcc(i,j) is minimum accumulated penalty incurred from start to state
    // tableRec(i,j) records the next state the optimal path took
    val tableVal = Table[Int](text, template, -1)
    val tableAcc = Table[Int](text, template, -1)
    val tableRec = Table[Int](text, template, -1)

    def inner(parentStep: Int, textIx: Int, templateIx: Int, acc: Int): Int = {
      stepCount += 1
      val step = stepCount

      printTraceMsg("enter", step, "from", parentStep, text, textIx, template, templateIx)

      def set(score: Int)  = score >= 0
      def templateFinished = templateIx >= template.length
      def textFinished     = textIx >= text.length
      def canFork          = set(forkIx)
      def forkIx           = flow.forks(templateIx)
      def mustGoto         = set(gotoIx)
      def gotoIx           = flow.gotos(templateIx)
      def nextIx           = templateIx+1
      def nextText         = textIx+1
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

      if (templateFinished && textFinished) {
        result = 0
      }
      else if (templateFinished && !textFinished) {
        attempt(nextText, templateIx, 1)
      }
      else if (mustGoto) {
        attempt(textIx, gotoIx, 0)
      }
      else {
        if (canFork) {
           attempt(textIx, forkIx, 0)
        }

        if (templateControl) {
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
          attempt(nextText, templateIx, 1)
        }
      }

      if (set(result)) {
        tableVal(textIx, templateIx) = result
      }
      if (set(resTextIx) && set(resTemplateIx)) {
        tableRec(textIx, templateIx) = tableRec.tableIx(resTextIx, resTemplateIx)
      }

      printTraceMsg("leave", step, "for", parentStep, text, textIx, template, templateIx, "score", result)

      result
    }

    val score = inner(0, 0, 0, 0)

    Match(text, this, score, tableRec)
  }

  def printTraceMsg(action: String, step: Int, connector: String, lastStep: Int, text: String, textIx: Int, template: String, templateIx: Int): Unit  = {
    if (trace) {
      val splitText     = Util.indexed(text, textIx)
      val splitTemplate = Util.indexed(template, templateIx)
      print(Color.std)
      println(f"$action $step%3d $connector%4s $lastStep%3d: position: $splitText%12s pattern: $splitTemplate%8s")
    }
  }

  def printTraceMsg(action: String, step: Int, connector: String, lastStep: Int, text: String, textIx: Int, template: String, templateIx: Int, scoreType: String, score: Int): Unit  = {
    if (trace) {
      val splitText = Util.indexed(text, textIx)
      val splitTemplate = Util.indexed(template, templateIx)
      print(Color.leaveText)
      println(f"$action $step%3d $connector%4s $lastStep%3d: position: $splitText%12s pattern: $splitTemplate%8s, $scoreType $score%2d")
    }
  }
}

case class Match(text: String, template: Template, score: Int, tableRec: Table[Int]) {

  def matchedText: String =
    trace().sliding(2).collect {
      case List((textIxNow, templateIxNow), (textIxNext, templateIxNext)) if (textIxNext == textIxNow+1) && (templateIxNext == templateIxNow+1) && textIxNow < text.length =>
        text(textIxNow)
    }.mkString

  def trace(textIx: Int = 0, templateIx: Int = 0): List[(Int, Int)] = {
    // TODO put set somewhere accessible, or use scala's value types to represent index
    if (textIx >= 0 && templateIx >= 0) {
      val nextTableIx = tableRec(textIx, templateIx)
      (textIx -> templateIx) :: trace(tableRec.textIx(nextTableIx), tableRec.templateIx(nextTableIx))
    } else {
      Nil
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

  def textIx(tableIx: Int): Int =
    tableIx / templateLength

  def templateIx(tableIx: Int): Int =
    tableIx % templateLength
}

object Util {
  def indexed(text: String, ix: Int): String = {
    val spaced = text + " "
    spaced.substring(0, ix) + Color.focusBack + spaced(ix) + Color.stdBack + spaced.substring(ix+1)
  }
}

/** An array of possible forks and gotos for each position
  *
  * If we have a set fork, the possibilities at that index are to jump straight to
  * the fork index without consuming character, or consume character and increment index.
  *
  * If we have a set goto, we must jump to the goto index straight away. It's up to this
  * method to ensure we only assign this to control characters, as it will clobber any
  * real characters we assign this to. (We really need a logical representation of the
  * pattern rather than a raw string.)
  *
  * There are two basic patterns:
  *
  * index:   0123456789
  * pattern: ab(cd)*ef
  * forks:   ..7......
  * gotos:   ......3..
  *
  * index:   0123456
  * pattern: abc*de
  * forks:   ..4...
  * gotos:   ...2..
  *
  * forks at the beginning and end of the pattern are fine, as an index equal to length of
  * pattern is allowed to indicate we are at the end:
  *
  * index:   01234
  * pattern: abc*
  * forks:   ..4.
  * gotos:   ...2
  *
  * index:   01234
  * pattern: a*bc
  * forks:   2....
  * gotos:   .0...
  *
  * Unclear if unstructured forks and gotos are best structure, vs. something that understands nesting.
  */
case class ControlFlow(forks: Array[Int], gotos: Array[Int])

object ControlFlow {  
  def apply(template: String): ControlFlow = {
    val forks = Array.fill(template.length)(-1)
    val gotos = Array.fill(template.length)(-1)

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
          forks(scopeStart) = ix+2       // can fork past the asterix
          gotos(ix+1)       = scopeStart // jump back at the asterix, for consistency with a* case
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
          forks(ix)   = ix+2 // can fork past the asterix
          gotos(ix+1) = ix   // jump back at asterix, fork will take as past this point
          scopes

        case (c, _, _, _) =>
          scopes
      }
    }

    ControlFlow(forks, gotos)
  }
}

object Color {
  val stdText   = Console.WHITE
  val errText   = Console.RED
  val leaveText = Console.YELLOW

  val stdBack   = Console.BLACK_B
  val focusBack = Console.BLUE_B

  val std = stdText + stdBack
}
