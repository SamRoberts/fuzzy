package fuzzy

case class Match(text: String, pattern: Pattern, score: Int, tableRec: Table[Int]) {

  def matchedText: String =
    trace().sliding(2).collect {
      case List((textIxNow, patternIxNow), (textIxNext, patternIxNext)) if (textIxNext == textIxNow+1) && (patternIxNext == patternIxNow+1) && textIxNow < text.length =>
        text(textIxNow)
    }.mkString

  def trace(textIx: Int = 0, patternIx: Int = 0): List[(Int, Int)] = {
    // TODO put set somewhere accessible, or use scala's value types to represent index
    if (textIx >= 0 && patternIx >= 0) {
      val nextTableIx = tableRec(textIx, patternIx)
      (textIx -> patternIx) :: trace(tableRec.textIx(nextTableIx), tableRec.patternIx(nextTableIx))
    } else {
      Nil
    }
  }
}

