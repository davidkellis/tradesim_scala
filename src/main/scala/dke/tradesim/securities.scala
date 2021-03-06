package dke.tradesim

import dke.tradesim.core.{Exchange, Security}
import dke.tradesim.logger.info
import dke.tradesim.db.Adapter

import Adapter.dynamicAdapter

object securities {
  def findExchanges(exchangeLabels: Seq[String])(implicit adapter: Adapter): Seq[Exchange] = {
    info(s"findExchanges(${exchangeLabels.mkString(",")}})")
    adapter.findExchanges(exchangeLabels)
  }

  val Amex = findExchanges(List("UA"))
  val Nasdaq = findExchanges(List("UW", "UQ", "UR"))
  val Nyse = findExchanges(List("UN"))
  val PrimaryUsExchanges = Amex ++ Nasdaq ++ Nyse
  val OTC_BB = findExchanges(List("UU"))
  val OTC = findExchanges(List("UV"))

  def findSecurities(exchanges: Seq[Exchange], symbols: Seq[String])(implicit adapter: Adapter): Seq[Security] = {
    info(s"findSecurities(${exchanges.mkString(",")}}, ${symbols.mkString(",")}})")
    adapter.findSecurities(exchanges, symbols)
  }
}
