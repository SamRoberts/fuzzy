package fuzzy

object Main {

  def main(args: Array[String]): Unit = {
    val cases = List(
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

    val results = cases.map { case (pattern, text, expected) => (pattern,  text, expected, Pattern(pattern).score(text).score) }

    results.filter { case (_, _, expected, actual) => expected != actual }.foreach {
      case (pattern, text, expected, actual) =>
        Pattern(pattern).withTrace.score(text)
        print(Color.std)
        println(s"final score $actual does not equal $expected")
        println()
    }

    print(Color.std)
    println(" pattern  | text         | expected | actual ")
    println("----------|--------------|----------|--------")
    println(
      results.map { case (pattern, text, expected, actual) =>
        val color = if (expected == actual) Color.stdText else Color.errText
        f"$color $pattern%8s | $text%12s | $expected%8d | $actual%6d "
      }.mkString("\n")
    )
  }
}

