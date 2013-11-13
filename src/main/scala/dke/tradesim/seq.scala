package dke.tradesim

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
}
