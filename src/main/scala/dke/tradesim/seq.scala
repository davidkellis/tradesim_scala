package dke.tradesim

import scala.collection.immutable.LinearSeq

object seq {
  // example:
  // tabulate(5,2)( x => (x, x*2) ).take(10)
  // => Stream( (5,10), (7,14), (9, 18), ...)
  def tabulate[A](start: Int = 1, step: Int = 1)(f: Int => A): Stream[A] = {
    def gen(i: Int): Stream[A] = {
      f(i) #:: gen(i + step)
    }
    gen(start)
  }

  def liftOption[T](seqO: IndexedSeq[Option[T]]): Option[IndexedSeq[T]] = {
    val flatSeq = seqO.flatten
    if (flatSeq.length == seqO.length) Some(flatSeq)
    else None
  }

  def liftOption[T](seqO: LinearSeq[Option[T]]): Option[LinearSeq[T]] = {
    if (seqO.forall(_.isDefined)) Some(seqO.flatten)
    else None
  }
}
