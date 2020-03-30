object Main {

  def main(args: Array[String]): Unit = {
    println(Template(List(Constant("aa"), Text(), Constant("bb"))).withTrace.score("afaffbb"))
  }
}

case class Template(atoms: List[TemplateAtom], trace: Boolean = false) {

  def withTrace: Template = copy(trace = true)

  // TODO implement matching
  // TODO add regex like repeating patterns rather that just wildcards
  //      do this sooner rather than later in case it stuffs up logic
  // TODO pretty inefficient and easily runs out of stack space
  // TODO use template and templateIx instead of atoms and atomIx
  //      and use class to pre-calculate tableIx for pattern pos

  def score(text: String): Int = {
    var stepCount = 0

    val tableLen = tableIx(atoms, 0, text, 0) + 1 // TODO co-locate tableLen with tableIx
    val tableHit = Array.ofDim[Boolean](tableLen)
    val tableVal = Array.ofDim[Int](tableLen)

    def inner(atoms: List[TemplateAtom], textIx: Int, atomIx: Int): Int = {
      stepCount += 1
      val step = stepCount

      if (trace) {
        val split = s"${text.substring(0, textIx)}^${text.substring(textIx)}"
        val prettyAtoms = s"${atoms.take(1).map(pretty(_, atomIx)).mkString}${atoms.drop(1).map(pretty(_, 0)).mkString}"
        println(s"enter step $step: position: $split pattern: $prettyAtoms")
      }

      val tix = tableIx(atoms, atomIx, text, textIx)
      if (tableHit(tix)) {
        val result = tableVal(tix)

        if (trace) {
          val split = s"${text.substring(0, textIx)}^${text.substring(textIx)}"
          val prettyAtoms = s"${atoms.take(1).map(pretty(_, atomIx)).mkString}${atoms.drop(1).map(pretty(_, 0)).mkString}"
          println(s"leave step $step: position: $split pattern: $prettyAtoms, cached $result")
        }

        return result
      }

      val result = atoms match {
        case List() =>
          text.length - textIx

        case Constant(pattern) :: restAtoms if textIx == text.length =>
          pattern.length - atomIx + inner(restAtoms, textIx, 0)

        case Constant(pattern) :: restAtoms if textIx < text.length && atomIx == pattern.length =>
          inner(restAtoms, textIx, 0)

        case Constant(pattern) :: restAtoms if textIx < text.length && atomIx < pattern.length && pattern(atomIx) == text(textIx) =>
          // I believe I can prove that it's always optimal, or at least equal to optimal, to follow this path if text and pattern match
          inner(atoms, textIx+1, atomIx+1)

        case Constant(pattern) :: restAtoms if textIx < text.length && atomIx < pattern.length && pattern(atomIx) != text(textIx) =>
          1 + (inner(atoms, textIx+1, atomIx) min inner(atoms, textIx, atomIx+1))

        case Text() :: restAtoms if textIx == text.length =>
          inner(restAtoms, textIx, 0)

        case Text() :: restAtoms if textIx < text.length =>
          inner(atoms, textIx+1, atomIx) min inner(restAtoms, textIx, 0)
      }

      tableVal(tix) = result
      tableHit(tix) = true

      if (trace) {
        val split = s"${text.substring(0, textIx)}^${text.substring(textIx)}"
        val prettyAtoms = s"${atoms.take(1).map(pretty(_, atomIx)).mkString}${atoms.drop(1).map(pretty(_, 0)).mkString}"
        println(s"leave step $step: position: $split pattern: $prettyAtoms, score $result")
      }

      result
    }

    inner(atoms, 0, 0)
  }

  def tableIx(atoms: List[TemplateAtom], atomIx: Int, text: String, textIx: Int): Int =
    tableIx(atoms, atomIx) * text.length + text.length - textIx

  def tableIx(atoms: List[TemplateAtom], atomIx: Int): Int = {
    val head = atoms.take(1).map {
      case Constant(text) => text.length - atomIx
      case Text()         => 1
    }.sum

    val tail = atoms.drop(1).map {
      case Constant(text) => text.length
      case Text()         => 1
    }.sum

    head + tail
  }

  def pretty(atom: TemplateAtom, atomIx: Int): String =
    atom match {
      case Constant(text) => text.substring(atomIx)
      case Text()         => "*"
    }
}

sealed trait TemplateAtom
case class Constant(text: String) extends TemplateAtom
case class Text() extends TemplateAtom
