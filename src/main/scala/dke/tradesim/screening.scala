package dke.tradesim

import dke.tradesim.core.Security

object screening {
  // example: filter(securities, _.id.flatMap(id => totalDebt(datetime(t), id).map(_ == 0) ))
  // example: filter(securities, for(id <- _.id; debt <- totalDebt(datetime(t), id)) yield debt == 0 )
  def filter(securities: Seq[Security], pred: Security => Option[Boolean]): Seq[Security] = {
    securities.filter(pred(_).getOrElse(false))
  }

  // example: rank(securities, Seq(Orderering.by{ s: Security => marketCap(s.id) }))
  def rank(securities: Seq[Security], orderings: Seq[Ordering[Security]]): Seq[Security] =
    rankMap(securities, orderings).toIndexedSeq.sortBy(securityRankPair => securityRankPair._2).map(_._1)

  // example: rank(securities, Seq(Orderering.by{ s: Security => marketCap(s.id) }))
  //       => {security1 -> 3, security4 -> 2, security1 -> 8, ...}
  def rankMap(securities: Seq[Security], orderings: Seq[Ordering[Security]]): Map[Security, Int] = {
    val securityRankPairs = orderings.flatMap(securities.sorted(_).zipWithIndex)
    securityRankPairs.foldLeft(Map[Security,Int]()) { (memo, pair) =>
      val (security, rank) = pair
      val newRank = memo.getOrElse(security, 0) + rank
      memo + (security -> newRank)
    }
  }
}
