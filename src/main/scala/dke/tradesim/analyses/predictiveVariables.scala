package dke.tradesim.analyses

import org.joda.time.{LocalDate, DateTime}

import dke.tradesim.core.{Bar, QuarterlyReport, SecurityId, extractNumericAttributes}
import dke.tradesim.datetimeUtils.{date, datetime, interspersedDateSeries, days}
import dke.tradesim.{quotes, quarterlyReports}
import dke.tradesim.priceHistory.{priceHistoryInterval}
import dke.tradesim.schedule.{defaultHolidaySchedule, defaultTradingSchedule, buildTradingSchedule, isTradingDay, TradingSchedule}

object predictiveVariables {
  case class AttributeSetSnapshot(attributes: Map[String, BigDecimal], futureReturns: Map[String, BigDecimal])
  case class CompanySnapshot(quarterlyReport: QuarterlyReport, eodBar: Bar)

  def companySnapshot(time: DateTime, securityId: SecurityId): Option[CompanySnapshot] = {
    for {
      quarterlyReport <- quarterlyReports.findQuarterlyReport(time, securityId)
      eodBar <- quotes.findEodBar(time, securityId)
    } yield CompanySnapshot(quarterlyReport, eodBar)
  }

  def snapshotOfAttributes(time: DateTime, securityId: SecurityId): Option[AttributeSetSnapshot] = {
    val snapshot = companySnapshot(time, securityId)
    snapshot.map { snapshot =>
      val attributes = extractNumericAttributes(snapshot.quarterlyReport.incomeStatement) ++
                       extractNumericAttributes(snapshot.quarterlyReport.balanceSheet) ++
                       extractNumericAttributes(snapshot.quarterlyReport.cashFlowStatement)
      // todo, finish this
//      val futureReturns = queryForFutureReturns()
      val futureReturns = Map[String, BigDecimal]()
      AttributeSetSnapshot(attributes, futureReturns)
    }
  }

  def tradingDays(securityId: SecurityId, tradingSchedule: TradingSchedule): Stream[LocalDate] = {
    val securityPriceHistoryInterval = priceHistoryInterval(securityId)
    securityPriceHistoryInterval.map { interval =>
      interspersedDateSeries(date(interval.getStart), date(interval.getEnd), days(1)).filter(date => isTradingDay(date, tradingSchedule))
    }.getOrElse(Stream.empty[LocalDate])
  }

  def attributeSeries(securityId: SecurityId,
                      tradingSchedule: TradingSchedule,
                      attributeGetterFn: (SecurityId, DateTime) => BigDecimal): Stream[BigDecimal] = {
    tradingDays(securityId, tradingSchedule).map(date => attributeGetterFn(securityId, datetime(date)))
  }
}
