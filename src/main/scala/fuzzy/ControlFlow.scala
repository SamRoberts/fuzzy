package fuzzy

/** An array of possible forks and gotos for each position
  *
  * If we have a set fork, the possibilities at that index are to jump straight to
  * the fork index without consuming character, or consume character and increment index.
  *
  * If we have a set goto, we must jump to the goto index straight away. It's up to this
  * method to ensure we only assign this to control characters, as it will clobber any
  * real characters we assign this to. (We really need a logical representation of the
  * pattern rather than a raw string.)
  *
  * There are two basic patterns:
  *
  * index:   0123456789
  * pattern: ab(cd)*ef
  * forks:   ..7......
  * gotos:   ......3..
  *
  * index:   0123456
  * pattern: abc*de
  * forks:   ..4...
  * gotos:   ...2..
  *
  * forks at the beginning and end of the pattern are fine, as an index equal to length of
  * pattern is allowed to indicate we are at the end:
  *
  * index:   01234
  * pattern: abc*
  * forks:   ..4.
  * gotos:   ...2
  *
  * index:   01234
  * pattern: a*bc
  * forks:   2....
  * gotos:   .0...
  *
  * Unclear if unstructured forks and gotos are best structure, vs. something that understands nesting.
  */
case class ControlFlow(forks: Array[Int], gotos: Array[Int])

object ControlFlow {  
  def apply(pattern: String): ControlFlow = {
    val forks = Array.fill(pattern.length)(-1)
    val gotos = Array.fill(pattern.length)(-1)

    pattern.zipWithIndex.foldLeft(List.empty[Int]) { (scopes: List[Int], elem: (Char, Int)) =>
      val (c, ix) = elem
      (c, ix, ix+1 < pattern.length &&  pattern(ix+1) == '*', scopes) match {

        case ('(', ix, true,  _) =>
          throw new Exception(s"illegal (* pattern at ${Util.indexed(pattern, ix)}")

        case ('(', ix, false, _) =>
          ix +: scopes

        case (')', ix, _, Nil) =>
          throw new Exception(s"illegal ) at top level at ${Util.indexed(pattern, ix)}")

        case (')', ix, true, scopeStart :: lowerScopes) =>
          forks(scopeStart) = ix+2       // can fork past the asterix
          gotos(ix+1)       = scopeStart // jump back at the asterix, for consistency with a* case
          lowerScopes

        case (')', _, false, _ :: lowerScopes) =>
          lowerScopes

        case ('*', 0, _, _) =>
          throw new Exception(s"illegal * at ${Util.indexed(pattern, 0)}")

        case ('*', ix, true, _) =>
          throw new Exception(s"illegal ** pattern at ${Util.indexed(pattern, ix)}")

        case ('*', _, false, _) =>
          scopes

        case (c, ix, true, _) =>
          forks(ix)   = ix+2 // can fork past the asterix
          gotos(ix+1) = ix   // jump back at asterix, fork will take as past this point
          scopes

        case (c, _, _, _) =>
          scopes
      }
    }

    ControlFlow(forks, gotos)
  }
}

