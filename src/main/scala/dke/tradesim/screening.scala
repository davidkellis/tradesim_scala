package dke.tradesim

object screening {
  // example: filter(securities, _.id.flatMap(id => totalDebt(datetime(t), id).map(_ == 0) ))
  // example: filter(securities, for(id <- _.id; debt <- totalDebt(datetime(t), id)) yield debt == 0 )
  // exapmle: filter(securityIds) { securityId => mrq.marketCapitalization(time, securityId).map(_ >= marketCap) }
  def filter[T](objects: Seq[T])(pred: T => Option[Boolean]): Seq[T] = objects.filter(pred(_).getOrElse(false))

  // example:
  // rank(filteredSecurityIds,
  //      Seq(Ordering.by { securityId: SecurityId => mrq.greenblattEarningsYield(time, securityId) },
  //          Ordering.by { securityId: SecurityId => mrq.greenblattReturnOnCapital(time, securityId) }))
  def rank[T](objects: Seq[T], orderings: Seq[Ordering[T]]): IndexedSeq[T] =
    rankMap(objects, orderings).toIndexedSeq.sortBy(objectRankPair => objectRankPair._2).map(_._1)

  // example:
  // rank(filteredSecurityIds,
  //      Map(Ordering.by { securityId: SecurityId => mrq.greenblattEarningsYield(time, securityId) } -> 1,
  //          Ordering.by { securityId: SecurityId => mrq.greenblattReturnOnCapital(time, securityId) -> 2 }))
  def rank[T](objects: Seq[T], weightedOrderings: Map[Ordering[T], Int]): IndexedSeq[T] =
    rankMap(objects, weightedOrderings).toIndexedSeq.sortBy(objectRankPair => objectRankPair._2).map(_._1)

  // example:
  // rank(securities, Seq(Orderering.by{ s: Security => marketCap(s.id) }))
  // => {security1 -> 3, security4 -> 2, security1 -> 8, ...}
  def rankMap[T](objects: Seq[T], orderings: Seq[Ordering[T]]): Map[T, Int] = {
    val objectRankPairs = orderings.flatMap(objects.sorted(_).zipWithIndex)
    objectRankPairs.foldLeft(Map[T, Int]()) { (memo, pair) =>
      val (obj, rank) = pair
      val newRank = memo.getOrElse(obj, 0) + rank
      memo + (obj -> newRank)
    }
  }

  // weightedOrderings is a Map[Ordering, Int] where the value is an integer rank multiplier; smaller rank multipliers indicate
  //   greater importance while larger rank multipliers indicate lesser importance. A rank multiplier of 1 means that
  //
  // example (greenblattEarningsYield is more important than greenblattReturnOnCapital):
  // rankMap(filteredSecurityIds,
  //         Map(Ordering.by { securityId: SecurityId => mrq.greenblattEarningsYield(time, securityId) } -> 1,
  //             Ordering.by { securityId: SecurityId => mrq.greenblattReturnOnCapital(time, securityId) -> 2 }))
  def rankMap[T](objects: Seq[T], weightedOrderings: Map[Ordering[T], Int]): Map[T, Int] = {
    val objectRankPairs = weightedOrderings.toList.flatMap { pair =>
      val (ordering, rankMultiplier) = pair
      objects.sorted(ordering).zip(seq.tabulate(1)(rank => rank * rankMultiplier))
    }
    objectRankPairs.foldLeft(Map[T, Int]()) { (memo, pair) =>
      val (obj, rank) = pair
      val newRank = memo.getOrElse(obj, 0) + rank
      memo + (obj -> newRank)
    }
  }
}
