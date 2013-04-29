package dke.tradesim

import org.joda.time.DateTime
import dke.tradesim.datetime.isInstantBetweenInclusive
import dke.tradesim.core.{Bar, Portfolio}
import dke.tradesim.quotes.{findEodBar}
import dke.tradesim.splits_dividends.{adjustPriceForCorporateActions}

object portfolio {
  type BarQuoteFn = (Bar) => BigDecimal

  def portfolioValue(portfolio: Portfolio, time: DateTime, priorBarPriceFn: BarQuoteFn, currentBarPriceFn: BarQuoteFn): BigDecimal = {
    val stockValues = for((symbol, qty) <- portfolio.stocks) yield stockValue(symbol, qty, time, priorBarPriceFn, currentBarPriceFn)
    val totalValue = stockValues.flatten.reduce(_ + _)
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