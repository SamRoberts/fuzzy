object Main {

  def main(args: Array[String]): Unit = {
    println(Template(Array(Constant("aa"), Text(), Constant("bb"), Finish())).withTrace.score("afaffbb"))
  }
}

case class Template(atoms: Array[TemplateAtom], trace: Boolean = false) {

  def withTrace: Template = copy(trace = true)

  // TODO add regex like repeating patterns rather that just wildcards
  //      do this sooner rather than later in case it stuffs up logic
  // TODO use template and templateIx instead of atoms and atomIx
  //      and use class to pre-calculate tableIx for pattern pos
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
    val index = Index(text, this)
    var stepCount = 0

    val tableHit = index.createTable[Boolean](false)
    val tableVal = index.createTable[Int](0)

    def inner(textIx: Int, templateIx: Int): Int = {
      stepCount += 1
      val step = stepCount

      if (trace) {
        val splitText = index.prettyText(textIx)
        val splitTemplate = index.prettyTemplate(templateIx)
        println(s"enter step $step: position: $splitText pattern: $splitTemplate")
      }

      val tix = index.tableIx(textIx, templateIx)
      if (tableHit(tix)) {
        val result = tableVal(tix)

        if (trace) {
          val splitText = index.prettyText(textIx)
          val splitTemplate = index.prettyTemplate(templateIx)
          println(s"leave step $step: position: $splitText pattern: $splitTemplate, cached $result")
        }

        return result
      }

      val result = index.atom(templateIx) match {
        case Constant(pattern) if textIx == text.length =>
          val atomIx = index.atomIx(templateIx)
          val jump   = pattern.length - atomIx
          jump + inner(textIx, templateIx+jump)

        case Constant(pattern) if textIx < text.length =>
          val atomIx = index.atomIx(templateIx)
          if (pattern(atomIx) == text(textIx)) {
            // I believe I can prove that it's always optimal, or at least equal to optimal, to follow this path if text and pattern match
            inner(textIx+1, templateIx+1)
          } else {
            1 + (inner(textIx+1, templateIx) min inner(textIx, templateIx+1))
          }

        case Text() if textIx == text.length =>
          inner(textIx, templateIx+1)

        case Text() if textIx < text.length =>
          inner(textIx+1, templateIx) min inner(textIx, templateIx+1)

        case Finish() =>
          // correct  whether textIx == or < than text.length
          text.length - textIx
      }

      tableVal(tix) = result
      tableHit(tix) = true

      if (trace) {
        val splitText = index.prettyText(textIx)
        val splitTemplate = index.prettyTemplate(templateIx)
        println(s"leave step $step: position: $splitText pattern: $splitTemplate, score $result")
      }

      result
    }

    inner(0, 0)
  }
}

/** A class which understands how to index into templates and text and template vs. text tables. */
case class Index(text: String, template: Template) {
  val templateSizes = template.atoms.map {
                        case Constant(c) => c.length // have to be careful I don't assume that incrementing templateIx by 1 doesn't skip empty constants
                        case Text()      => 1
                        case Finish()    => 1
                      }
  val templateOuterIx: Array[Int] = templateSizes.zipWithIndex.flatMap { case (n,i) => Array.fill(n)(i) }
  val templateInnerIx: Array[Int] = templateSizes.flatMap(0 until _)

  val templateLength: Int = templateInnerIx.length
  val textLength: Int = text.length + 1 // textIx can be length of text, to represent consumption is complete

  val tableLength = templateLength * textLength

  def createTable[T : scala.reflect.ClassTag](element: T): Array[T] = Array.fill(tableLength)(element)

  def tableIx(textIx: Int, templateIx: Int): Int =
    textIx * templateLength + templateIx

  def atom(templateIx: Int): TemplateAtom = template.atoms(templateOuterIx(templateIx))

  def atomIx(templateIx: Int): Int = templateInnerIx(templateIx)

  def prettyText(textIx: Int): String =
    s"${text.substring(0, textIx)}^${text.substring(textIx)}"

  def prettyTemplate(templateIx: Int): String = {
    val outerIx = templateOuterIx(templateIx)
    val innerIx = templateInnerIx(templateIx)
    template.atoms.zipWithIndex.map {
      case (Constant(c), i) if i  == outerIx => s"${c.substring(0, innerIx)}^${c.substring(innerIx)}"
      case (Constant(c), _)                  => c
      case (Text(), 0)                       => "*"
       case (Finish(), 0)                    => "$"
    }.mkString
  }
}

sealed trait TemplateAtom

case class Constant(text: String) extends TemplateAtom

case class Text() extends TemplateAtom

/** It's convenient to represent the end of the atom list with a value.
    It's an error if template atom list doesn't end with Finish. */
case class Finish() extends TemplateAtom
