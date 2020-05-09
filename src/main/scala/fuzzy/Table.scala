package fuzzy

/** A class which understands how to index into patterns and text and pattern vs. text tables. */
case class Table[T : scala.reflect.ClassTag](text: String, pattern: String, element: T) {

  val patternLength: Int = pattern.length + 1 // patternIx can be length of pattern, to represent consumption is complete
  val textLength: Int = text.length + 1 // textIx can be length of text, to represent consumption is complete

  val elems = Array.fill(textLength * patternLength)(element)

  def apply(textIx: Int, patternIx: Int): T =
     elems(tableIx(textIx, patternIx))

   def update(textIx: Int, patternIx: Int, elem: T): Unit =
     elems(tableIx(textIx, patternIx)) = elem

  def tableIx(textIx: Int, patternIx: Int): Int =
    textIx * patternLength + patternIx

  def textIx(tableIx: Int): Int =
    tableIx / patternLength

  def patternIx(tableIx: Int): Int =
    tableIx % patternLength
}

