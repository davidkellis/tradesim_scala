package dke.tradesim

import org.joda.time.DateTime
import dke.tradesim.datetimeUtils.isInstantBetweenInclusive
import dke.tradesim.core.{Bar, Portfolio}
import dke.tradesim.quotes.{findEodBar}
import dke.tradesim.splitsDividends.{adjustPriceForCorporateActions}

object portfolio {
  type BarQuoteFn = (Bar) => BigDecimal

  def portfolioValue(portfolio: Portfolio, time: DateTime, priorBarPriceFn: BarQuoteFn, currentBarPriceFn: BarQuoteFn): BigDecimal = {
//    val stockValues = for((symbol, qty) <- portfolio.stocks)
//                      yield stockValue(symbol, qty, time, priorBarPriceFn, currentBarPriceFn).getOrElse(BigDecimal(0))
    val stockValues = portfolio.stocks.map({ case (symbol, qty) => stockValue(symbol, qty, time, priorBarPriceFn, currentBarPriceFn).getOrElse(BigDecimal(0)) })
    val totalValue = stockValues.foldLeft(BigDecimal(0))(_ + _)
    portfolio.cash + totalValue
  }

  def stockValue(symbol: String, qty: Long, time: DateTime, priorBarPriceFn: BarQuoteFn, currentBarPriceFn: BarQuoteFn): Option[BigDecimal] = {
    val bar = findEodBar(time, symbol)
    bar.map { bar =>
      val priceFn = if (isInstantBetweenInclusive(time, bar.startTime, bar.endTime)) currentBarPriceFn else priorBarPriceFn
      val price = priceFn(bar)
      val priceObservationTime = bar.endTime
      val sharePrice = adjustPriceForCorporateActions(price, symbol, priceObservationTime, time)
      qty * sharePrice
    }
  }
}