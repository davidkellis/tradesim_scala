package dke.tradesim

import dke.tradesim.core.Security

object screening {
  // example: filter(companies, _.id.flatMap(id => totalDebt(datetime(t), id).map(_ == 0) ))
  // example: filter(companies, for(id <- _.id; debt <- totalDebt(datetime(t), id)) yield debt == 0 )
  def filter(securities: Seq[Security], pred: Security => Option[Boolean]): Seq[Security] = {
    securities.filter(pred(_).getOrElse(false))
  }

  def filter(securities: Seq[Security], pred: Security => Boolean): Seq[Security] = {
    securities.filter(pred(_))
  }

  def rank(securities: Seq[Security], orderings: Seq[Ordering[Security]]): Seq[Security] = {
    val securityRankPairs = orderings.flatMap(securities.sorted(_).zipWithIndex)
    val securityRankMap = securityRankPairs.foldLeft(Map[Security,Int]()) { (memo, pair) =>
      val (security, rank) = pair
      val newRank = memo.getOrElse(security, 0) + rank
      memo + (security -> newRank)
    }
    securityRankMap.toIndexedSeq.sortBy(securityRankPair => securityRankPair._2).map(_._1)
  }

  def test() {
    val securities = Seq[Security]()
    val o1 = Ordering.by {s: Security => s.bbGcid }
    val o2 = Ordering.by {s: Security => s.bbGid }
    rank(securities, Seq(o1, o2))
  }
}
