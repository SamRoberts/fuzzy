package fuzzy.impl

sealed trait Pattern {
  def score(text: String): Match =
    Pattern.score(this, text.toList, Nil, false)
}

case object End extends Pattern
case class Lit(char: Char, next: Pattern) extends Pattern
case class Any(next: Pattern) extends Pattern
case class Push(arity: Arity, start: Pattern, after: Pattern) extends Pattern
case object Pop extends Pattern

object Pattern {

  def apply(pattern: String): Pattern = {
    val (rest, parsed) = parse(pattern.toList, 0)(identity)
    if (rest.nonEmpty) throw new Exception(s"end of string not in a pattern: $rest")
    parsed
  }

  def parse(pattern: List[Char], depth: Int)(f: Pattern => Pattern): (List[Char], Pattern) = {
    val (rest, parsed) = pattern match {
      case '*' :: _                  => throw new Exception("unexepcted asterix")
      case Nil         if depth >= 1 => throw new Exception("unclosed brackets")
      case ')' :: rest if depth == 0 => throw new Exception("unmatched closing bracket")

      case Nil         if depth == 0 => (Nil, End)
      case ')' :: rest if depth >= 1 => (rest, Pop)

      case '(' :: rest               => parse(rest, depth+1)(identity) match {
        case ('*' :: after, inside)    => parse(after, depth)(Push(Many, inside, _))
        case (after,        inside)    => parse(after, depth)(Push(One,  inside, _))
      }
      case '.' :: '*' :: rest        => parse(rest, depth)(Push(Many, Any(Pop), _))
      case c   :: '*' :: rest        => parse(rest, depth)(Push(Many, Lit(c, Pop), _))

      case '.' :: rest               => parse(rest, depth)(Any(_))
      case c :: rest                 => parse(rest, depth)(Lit(c, _))
    }

    (rest, f(parsed))
  }

  def score(pattern: Pattern, text: List[Char], env: List[Scope], progressed: Boolean): Match = (pattern, text, env) match {
    case (End,         Nil,        env)           => MatchStart
    case (End,          t :: rest, env)           => SkipText(t, score(End, rest, env, progressed))

    case (Lit(p, next), Nil,       env)           => SkipLit(p, score(next, Nil, env, progressed))
    case (Lit(p, next), t :: rest, env) if p == t => MatchChar(p, score(next, rest, env, true))
    case (Lit(p, next), t :: rest, env) if p != t => SkipLit(p, score(next, t :: rest, env, progressed)) or
                                                     SkipText(t, score(Lit(p, next), rest, env, true))

    case (Any(next),    Nil,       env)           => SkipAny(score(next, Nil, env, progressed))
    case (Any(next),    t :: rest, env)           => MatchChar(t, score(next, rest, env, true))

    case (Push(One,  start, after), _, env)       => Enter(score(start, text, Scope(One,  start, after, progressed) :: env, false))
    case (Push(Many, start, after), _, env)       => Enter(score(start, text, Scope(Many, start, after, progressed) :: env, false)) or
                                                       score(after, text, env, progressed)

    case (Pop, _, Scope(One,  _,     after, afterProgressed) :: env)                =>
      Leave(score(after, text, env, afterProgressed))
    case (Pop, _, Scope(Many, _,     after, afterProgressed) :: env) if !progressed =>
      Leave(score(after, text, env, afterProgressed))
    case (Pop, _, Scope(Many, start, after, afterProgressed) :: env) if progressed  =>
      score(start, text, Scope(Many, start, after, afterProgressed) :: env, false) or
      Leave(score(after, text, env, afterProgressed))
  }
}


sealed trait Arity
case object One extends Arity
case object Many extends Arity

case class Scope(arity: Arity, start: Pattern, after: Pattern, afterProgressed: Boolean)

object Scope {
  def apply(push: Push, progressed: Boolean): Scope = Scope(push.arity, push.start, push.after, progressed)
}

sealed trait Match {
  def score: Int

  def or(other: Match) = if (score <= other.score) this else other
}

case object MatchStart extends Match { val score = 0 }
case class MatchChar(c: Char, next: Match) extends Match { val score = next.score }
case class SkipText(c: Char, next: Match) extends Match { val score = 1 + next.score }
case class SkipLit(c: Char, next: Match) extends Match { val score = 1 + next.score }
case class SkipAny(next: Match) extends Match { val score = 1 + next.score }
case class Enter(next: Match) extends Match { val score = next.score }
case class Leave(next: Match) extends Match { val score = next.score }

