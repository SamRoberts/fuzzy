package fuzzy

// a dumb implementation of pattern to start with, will consider how to improve while implementing new predicatable algorithm

sealed trait Pattern

object Pattern {
  case object Any extends Pattern
  case class Lit(char: Char) extends Pattern
  case class Group(inside: Pattern) extends Pattern
  case class Kleene(inside: Pattern) extends Pattern
  case class Concat(elements: Array[Pattern]) extends Pattern

  implicit val factory = new PatternFactory[Pattern] {
    type Builder = Pattern

    def freeze(builder: Builder): Pattern = builder

    def any(text: String): Builder =
      Any

    def lit(char: Char, text: String): Builder =
      Lit(char)

    def group(inside: Builder, textPre: String, textPost: String): Builder =
      Group(inside)

    def kleene(inside: Builder, testPre: String, textPost: String): Builder =
      Kleene(inside)

    def concat(builders: Seq[Builder]): Builder =
      Concat(builders.toArray)
  }
}
