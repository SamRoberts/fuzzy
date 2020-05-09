package fuzzy

object Util {
  def indexed(text: String, ix: Int): String = {
    val spaced = text + " "
    spaced.substring(0, ix) + Color.focusBack + spaced(ix) + Color.stdBack + spaced.substring(ix+1)
  }
}

