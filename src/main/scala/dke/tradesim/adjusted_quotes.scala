package dke.tradesim

import org.joda.time.DateTime
import dke.tradesim.core.Bar
import dke.tradesim.quotes.{barClose, barOpen, barSimQuote}

object adjusted_quotes {
  def adjEodClose(time: DateTime, symbol: String): BigDecimal = adjEodQuote(time, symbol, barClose _)

  def adjEodOpen(time: DateTime, symbol: String): BigDecimal = adjEodQuote(time, symbol, barOpen _)

  def adjEodSimQuote(time: DateTime, symbol: String): BigDecimal = adjEodQuote(time, symbol, barSimQuote _)

  def adjEodQuote(time: DateTime, symbol: String, priceFn: Bar => BigDecimal): BigDecimal = {
    val bar = findEodBar(time, symbol)
    val price = priceFn(bar)
    val priceObservationTime = bar.endTime
    adjustPriceForCorporateActions(price, symbol, priceObservationTime, time)
  }


  def adjEodClosePriorTo(time: DateTime, symbol: String): BigDecimal = adjEodQuotePriorTo(time, symbol, barClose _)

  def adjEodOpenPriorTo(time: DateTime, symbol: String): BigDecimal = adjEodQuotePriorTo(time, symbol, barOpen _)

  def adjEodSimQuotePriorTo(time: DateTime, symbol: String): BigDecimal = adjEodQuotePriorTo(time, symbol, barSimQuote _)

  def adjEodQuotePriorTo(time: DateTime, symbol: String, priceFn: Bar => BigDecimal): BigDecimal = {
    val bar = findEodBarPriorTo(time, symbol)
    val price = priceFn(bar)
    val priceObservationTime = bar.endTime
    adjustPriceForCorporateActions(price, symbol, priceObservationTime, time)
  }
}