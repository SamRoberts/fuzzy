object Main {

  def main(args: Array[String]): Unit = {
    val cases = List(
      ("", "", 0),
      ("", "aabb", 4),
      ("aa", "", 2),
      (".*", "aabb", 0),
      ("aa", "aba", 1),
      ("a.a", "aba", 0),
      ("aa.", "aba", 2),
      ("aa.*bb", "afaffbb", 1),
      ("a..*bb", "afaffbb", 0)
    )

    val results = cases.map { case (template, text, expected) => (template,  text, expected, Template(template).score(text)) }

    results.filter { case (_, _, expected, actual) => expected != actual }.foreach {
      case (template, text, expected, actual) =>
        Template(template).withTrace.score(text)
        println(s"final score $actual does not equal $expected")
        println()
    }

    println(" template | text     | expected | actual ")
    println("----------|----------|----------|--------")
    println(
      results.map { case (template, text, expected, actual) =>
        f" $template%8s | $text%8s | $expected%8d | $actual%6d "
      }.mkString("\n")
    )
  }
}

case class Template(template: String, trace: Boolean = false) {

  val forks = createForks(template)

  def withTrace: Template = copy(trace = true)

  // TODO add regex like repeating patterns rather that just wildcards
  //      do this sooner rather than later in case it stuffs up logic
  // TODO implement matching. Could be higher priority, but how to implement matching?
  //      Can we record at each table index what the index of the next step we took was?
  //      Then how to recover match from that?
  //      Seems to me this is where Template class with pattern <=> tableIx comes in handy
  //      But want proper repeating patterns first in case it stuffs up table logic
  //      So do repeating patterns first, then template/templateIx change, then matching
  // TODO pretty inefficient and easily runs out of stack space
  // TODO get benchmarks up and running and then start investigating whether we can make
  //      code more optimised and safer. Current approach is both overly simplistic but also
  //      more skeptical of creating new objects than standard Scala code.
  // TODO Atom should be Segment or similar rather than atom ... constant is not indivisible!

  def score(text: String): Int = {
    var stepCount = 0

    val tableHit = Table[Boolean](text, template, false)
    val tableVal = Table[Int](text, template, 0)

    def inner(textIx: Int, templateIx: Int): Int = {
      stepCount += 1
      val step = stepCount

      printTraceMsg("enter", step, text, textIx, template, templateIx)

      if (tableHit(textIx, templateIx)) {
        val result = tableVal(textIx, templateIx)
        printTraceMsg("leave", step,  text, textIx, template, templateIx, "cache", result)
        return result
      }

      val result = {
        // TODO figure out how to incorporate forks array into calculation
        // now we:
        //   - template and text are at an end, finish
        //   - if template is control char, skip template
        //   - if template does not match text, take penality plus minimum of skipping template or text
        //   - if template matches text, skip template and text
        //   - when skipping template with fork, take minimum of incrementing or forking
        //   - when skipping text or skipping template with no fork, just increment
        //   - any skipping options that can't be done due to end of text or template ... well, obviously can't be done

        if (templateIx < template.length && template(templateIx) == '*' && textIx == text.length) {
          inner(textIx, templateIx+1)
        }
        else if (templateIx < template.length && template(templateIx) == '*' && textIx < text.length) {
          inner(textIx+1, templateIx) min inner(textIx, templateIx+1)
        }
        else if (templateIx < template.length && textIx == text.length) {
          1 + inner(textIx, templateIx+1)
        }
        else if (templateIx < template.length && textIx < text.length) {
          if (template(templateIx)  == '.' || template(templateIx) == text(textIx)) {
            // I believe I can prove that it's always optimal, or at least equal to optimal, to follow this path char can consume something
            // although for the purposes of cleanness it might be nicer to miss a character wildcard than a constant character
            // something about going with the optiomal path which had the least chance of being optimal? maybe?
            inner(textIx+1, templateIx+1)
          } else {
            1 + (inner(textIx+1, templateIx) min inner(textIx, templateIx+1))
          }
        }
        else if (templateIx == template.length) {
          // correct  whether textIx == or < than text.length
          text.length - textIx
        }
        else {
          throw new Exception("programmer error: unexpected case")
        }
      }

      tableVal(textIx, templateIx) = result
      tableHit(textIx, templateIx) = true

      printTraceMsg("leave", step, text, textIx, template, templateIx, "score", result)

      result
    }

    inner(0, 0)
  }

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

  def printTraceMsg(action: String, step: Int, text: String, textIx: Int, template: String, templateIx: Int): Unit  = {
    if (trace) {
      val splitText = Util.indexed(text, textIx)
      val splitTemplate = Util.indexed(template, templateIx)
      println(f"$action step $step%3d: position: $splitText%8s pattern: $splitTemplate%8s")
    }
  }

  def printTraceMsg(action: String, step: Int, text: String, textIx: Int, template: String, templateIx: Int, scoreType: String, score: Int): Unit  = {
    if (trace) {
      val splitText = Util.indexed(text, textIx)
      val splitTemplate = Util.indexed(template, templateIx)
      println(f"$action step $step%3d: position: $splitText%8s pattern: $splitTemplate%8s, $scoreType $score%2d")
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
  def indexed(text: String, ix: Int): String =
    s"${text.substring(0, ix)}^${text.substring(ix)}"
}
