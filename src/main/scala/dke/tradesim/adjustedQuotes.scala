package dke.tradesim

import org.joda.time.DateTime
import dke.tradesim.core.{SecurityId, Bar}
import dke.tradesim.quotes.{barClose, barOpen, barSimQuote, findEodBar, findEodBarPriorTo}
import dke.tradesim.splitsDividends.{adjustPriceForCorporateActions}

object adjustedQuotes {
  def adjEodClose(time: DateTime, securityId: SecurityId): Option[BigDecimal] = adjEodQuote(time, securityId, barClose _)

  def adjEodOpen(time: DateTime, securityId: SecurityId): Option[BigDecimal] = adjEodQuote(time, securityId, barOpen _)

  def adjEodSimQuote(time: DateTime, securityId: SecurityId): Option[BigDecimal] = adjEodQuote(time, securityId, barSimQuote _)

  def adjEodQuote(time: DateTime, securityId: SecurityId, priceFn: Bar => BigDecimal): Option[BigDecimal] = {
    val bar = findEodBar(time, securityId)
    bar.map { bar =>
      val price = priceFn(bar)
      val priceObservationTime = bar.endTime
      adjustPriceForCorporateActions(price, securityId, priceObservationTime, time)
    }
  }


  def adjEodClosePriorTo(time: DateTime, securityId: SecurityId): Option[BigDecimal] = adjEodQuotePriorTo(time, securityId, barClose _)

  def adjEodOpenPriorTo(time: DateTime, securityId: SecurityId): Option[BigDecimal] = adjEodQuotePriorTo(time, securityId, barOpen _)

  def adjEodSimQuotePriorTo(time: DateTime, securityId: SecurityId): Option[BigDecimal] = adjEodQuotePriorTo(time, securityId, barSimQuote _)

  def adjEodQuotePriorTo(time: DateTime, securityId: SecurityId, priceFn: Bar => BigDecimal): Option[BigDecimal] = {
    val bar = findEodBarPriorTo(time, securityId)
    bar.map { bar =>
      val price = priceFn(bar)
      val priceObservationTime = bar.endTime
      adjustPriceForCorporateActions(price, securityId, priceObservationTime, time)
    }
  }
}