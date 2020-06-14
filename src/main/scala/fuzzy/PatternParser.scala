package fuzzy

import fastparse._, NoWhitespace._
import fastparse.{parse => fastparse}

object PatternParser {

  def parse[Patt: PatternFactory](text: String): Either[String, Patt] = {
    val factory = implicitly[PatternFactory[Patt]]

    fastparse(text, parser(_, factory)) match {
      case success : Parsed.Success[Patt] => Right(success.value)
      case failure : Parsed.Failure       => Left(failure.msg)
    }
  }

  def parser[_: P, Patt: PatternFactory]: P[Patt] = {
    val factory = implicitly[PatternFactory[Patt]]
    type Builder = factory.Builder

    val illegalLits = ".*()\\"

    def any: P[Builder]     = P(".").map(text => factory.any("."))
    def lit: P[Builder]     = P(CharPred(c => !illegalLits.contains(c)).!).map(text => factory.lit(text.head, text))
    def esc: P[Builder]     = P("\\" ~/ AnyChar.!).map(text => factory.lit(text.head, s"\\$text"))
    def group: P[Builder]   = P("(" ~/ pattern ~ ")").map(b => factory.group(b, "(", ")"))
    def single: P[Builder]  = P(group | lit | esc | any)
    def elem: P[Builder]    = (P(single ~ "*".!.?): P[(Builder, Option[String])]).map { case (b, Some(_)) => factory.kleene(b, "", "*"); case (b, None) => b }
    def pattern: P[Builder] = elem.rep.map(factory.concat _)

    P(pattern ~ End).map(factory.freeze _)
  }
}

/** Type class for constructing patterns. */
trait PatternFactory[Patt] {
  type Builder

  def freeze(builder: Builder): Patt

  def any(text: String): Builder
  def lit(char: Char, text: String): Builder
  def group(inside: Builder, textPre: String, textPost: String): Builder
  def kleene(inside: Builder, testPre: String, textPost: String): Builder
  def concat(builders: Seq[Builder]): Builder
}
