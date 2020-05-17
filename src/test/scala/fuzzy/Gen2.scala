package fuzzy

import hedgehog._
import hedgehog.predef._

/** generator utilities that should probably be in scala-hedgehog */
object Gen2 {
  /** Returns a list of elements randomly selected from elements.
   *
   *  Range cannot be larger than the number of elements.
   */
  def uniqList[A](elements: List[A], range: Range[Int]): Gen[List[A]] = {
    val maxSize = elements.length
    for {
      // technically order does not give a uniformly random order, if the same long is generated twice ...
      // not going to worry too much about that possibility
      order <- replicateM(maxSize, Gen.long(Range(0L, _ => Long.MinValue -> Long.MaxValue)))
      sorted = elements.zip(order).sortBy(_._2).map(_._1)
      size  <- Gen.int(range).filter(_ <= maxSize)
    } yield {
      sorted.take(size)
    }
  }

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
