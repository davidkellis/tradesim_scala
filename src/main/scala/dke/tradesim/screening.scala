package dke.tradesim

object screening {
  // example: filter(securities, _.id.flatMap(id => totalDebt(datetime(t), id).map(_ == 0) ))
  // example: filter(securities, for(id <- _.id; debt <- totalDebt(datetime(t), id)) yield debt == 0 )
  def filter[T](objects: Seq[T])(pred: T => Option[Boolean]): Seq[T] = objects.filter(pred(_).getOrElse(false))

  // example: rank(securities, Seq(Orderering.by{ s: Security => marketCap(s.id) }))
  def rank[T](objects: Seq[T], orderings: Seq[Ordering[T]]): IndexedSeq[T] =
    rankMap(objects, orderings).toIndexedSeq.sortBy(objectRankPair => objectRankPair._2).map(_._1)

  // example: rank(securities, Seq(Orderering.by{ s: Security => marketCap(s.id) }))
  //       => {security1 -> 3, security4 -> 2, security1 -> 8, ...}
  def rankMap[T](objects: Seq[T], orderings: Seq[Ordering[T]]): Map[T, Int] = {
    val objectRankPairs = orderings.flatMap(objects.sorted(_).zipWithIndex)
    objectRankPairs.foldLeft(Map[T, Int]()) { (memo, pair) =>
      val (obj, rank) = pair
      val newRank = memo.getOrElse(obj, 0) + rank
      memo + (obj -> newRank)
    }
  }
}
