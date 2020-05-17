package fuzzy

object Color {
  val stdText   = Console.WHITE
  val errText   = Console.RED
  val leaveText = Console.YELLOW

  val stdBack   = Console.BLACK_B
  val focusBack = Console.BLUE_B
  val errBack   = Console.RED_B

  val std = stdText + stdBack
}
