package fuzzy

import hedgehog._
import hedgehog.predef._

/** generator utilities that should probably be in scala-hedgehog */
object Gen2 {
  def uniqList[A](gen: Gen[A], range: Range[Int]): Gen[List[A]] = {
    // Not sure why Gen.list looks the way it does, should take a proper look and see if it's construction is relevant to this list too.
    def inner(avoid: Set[A], targetSize: Int, attempt: Int): Gen[List[A]] =
      if (targetSize <= 0)
        Gen.constant(Nil)
      else if (attempt >= 10)
        Gen.discard
      else
        for {
          rawElems  <- replicateM(targetSize, gen.filter(c => !avoid.contains(c)))
          newElems   = rawElems.distinct
          newAvoid   = avoid ++ newElems
          actualSize = newElems.length
          restElems <- if (actualSize == targetSize) Gen.constant(Nil)
                       else                          inner(newAvoid, actualSize, attempt+1)
        } yield {
          newElems ++ restElems
        }

    Gen.int(range).flatMap(size => inner(Set(), size, 0))
  }
}
