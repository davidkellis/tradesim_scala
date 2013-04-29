package dke.tradesim

import org.joda.time.DateTime
import dke.tradesim.core.Bar
import dke.tradesim.quotes.{barClose, barOpen, barSimQuote, findEodBar, findEodBarPriorTo}
import dke.tradesim.splits_dividends.{adjustPriceForCorporateActions}

object adjusted_quotes {
  def adjEodClose(time: DateTime, symbol: String): Option[BigDecimal] = adjEodQuote(time, symbol, barClose _)

  def adjEodOpen(time: DateTime, symbol: String): Option[BigDecimal] = adjEodQuote(time, symbol, barOpen _)

  def adjEodSimQuote(time: DateTime, symbol: String): Option[BigDecimal] = adjEodQuote(time, symbol, barSimQuote _)

  def adjEodQuote(time: DateTime, symbol: String, priceFn: Bar => BigDecimal): Option[BigDecimal] = {
    val bar = findEodBar(time, symbol)
    bar.map { bar =>
      val price = priceFn(bar)
      val priceObservationTime = bar.endTime
      adjustPriceForCorporateActions(price, symbol, priceObservationTime, time)
    }
  }


  def adjEodClosePriorTo(time: DateTime, symbol: String): Option[BigDecimal] = adjEodQuotePriorTo(time, symbol, barClose _)

  def adjEodOpenPriorTo(time: DateTime, symbol: String): Option[BigDecimal] = adjEodQuotePriorTo(time, symbol, barOpen _)

  def adjEodSimQuotePriorTo(time: DateTime, symbol: String): Option[BigDecimal] = adjEodQuotePriorTo(time, symbol, barSimQuote _)

  def adjEodQuotePriorTo(time: DateTime, symbol: String, priceFn: Bar => BigDecimal): Option[BigDecimal] = {
    val bar = findEodBarPriorTo(time, symbol)
    bar.map { bar =>
      val price = priceFn(bar)
      val priceObservationTime = bar.endTime
      adjustPriceForCorporateActions(price, symbol, priceObservationTime, time)
    }
  }
}