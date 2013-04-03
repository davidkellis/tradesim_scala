package dke.tradesim

import org.joda.time.DateTime
import dke.tradesim.datetime.isInstantBetweenInclusive
import dke.tradesim.core.Portfolio

object portfolio {
  type BarQuoteFn = (DayBar) => BigDecimal

  def stockValue(symbol: String, qty: Long, time: DateTime, priorBarPriceFn: BarQuoteFn, currentBarPriceFn: BarQuoteFn): BigDecimal = {
    val bar = findDayBar(time, symbol)
    val priceFn = if (isInstantBetweenInclusive(time, bar.startTime, bar.endTime)) currentBarPriceFn else priorBarPriceFn
    val price = priceFn(bar)
    val priceObservationTime = bar.endTime
    val sharePrice = adjustPriceForCorporateActions(price, symbol, priceObservationTime, time)
    qty * sharePrice
  }

  def portfolioValue(portfolio: Portfolio, time: DateTime, priorBarPriceFn: BarQuoteFn, currentBarPriceFn: BarQuoteFn): BigDecimal = {
    val valueOfSecurities: BigDecimal = (for((symbol, qty) <- portfolio.stocks) yield stockValue(symbol, qty, time, priorBarPriceFn, currentBarPriceFn)).reduce(_ + _)
    portfolio.cash + valueOfSecurities
  }
}