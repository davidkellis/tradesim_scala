package dke.tradesim.analyses

import org.joda.time.{Period, LocalDate, DateTime}

import dke.tradesim.core._
import dke.tradesim.datetimeUtils.{date, datetime, interspersedDateSeries, days}
import dke.tradesim.db.{Adapter}
import dke.tradesim.{quotes, quarterlyReports}
import dke.tradesim.priceHistory.{priceHistoryInterval}
import dke.tradesim.schedule.{defaultHolidaySchedule, defaultTradingSchedule, buildTradingSchedule, isTradingDay, TradingSchedule}
import dke.tradesim.core.QuarterlyReport

import Adapter.dynamicAdapter

object predictiveVariables {
  case class CompanyReportsSnapshot(quarterlyReport: Option[QuarterlyReport], eodBar: Option[Bar])
  case class AttributeSetSnapshot(time: DateTime, attributes: Map[String, BigDecimal], futureReturns: Map[String, BigDecimal])

  def attributeSetSnapshotSeries(securityId: SecurityId,
                                 tradingSchedule: TradingSchedule,
                                 timeInFutureOffsets: Seq[Period],
                                 principal: BigDecimal,
                                 commissionPerTrade: BigDecimal,
                                 commissionPerShare: BigDecimal): Stream[AttributeSetSnapshot] = {
    tradingDaysSpanningPriceHistory(securityId, tradingSchedule).map { date =>
      attributeSetSnapshot(securityId, datetime(date, 12, 0, 0), timeInFutureOffsets, principal, commissionPerTrade, commissionPerShare)
    }
  }

  //  def attributeSeries(securityId: SecurityId,
  //                      tradingSchedule: TradingSchedule,
  //                      attributeGetterFn: (DateTime, SecurityId) => BigDecimal): Stream[BigDecimal] = {
  //    tradingDaysSpanningPriceHistory(securityId, tradingSchedule).map(date => attributeGetterFn(datetime(date), securityId))
  //  }

  def tradingDaysSpanningPriceHistory(securityId: SecurityId, tradingSchedule: TradingSchedule): Stream[LocalDate] = {
    val securityPriceHistoryInterval = priceHistoryInterval(securityId)
    securityPriceHistoryInterval.map { interval =>
      interspersedDateSeries(date(interval.getStart), date(interval.getEnd), days(1)).filter(date => isTradingDay(date, tradingSchedule))
    }.getOrElse(Stream.empty[LocalDate])
  }

  def attributeSetSnapshot(securityId: SecurityId,
                           time: DateTime,
                           timeInFutureOffsets: Seq[Period],
                           principal: BigDecimal,
                           commissionPerTrade: BigDecimal,
                           commissionPerShare: BigDecimal): AttributeSetSnapshot = {
    val reportsSnapshot = companyReportsSnapshot(time, securityId)
    val attributes = extractNumericAttributesFromReportsSnapshot(reportsSnapshot)

    val futureReturns = queryForFutureReturns(securityId, time, timeInFutureOffsets, principal, commissionPerTrade, commissionPerShare)

    AttributeSetSnapshot(time, attributes, futureReturns)
  }

  def companyReportsSnapshot(time: DateTime, securityId: SecurityId): CompanyReportsSnapshot = {
    CompanyReportsSnapshot(
      quarterlyReports.findQuarterlyReport(time, securityId),
      quotes.findEodBar(time, securityId)
    )
  }


  def extractNumericAttributesFromReportsSnapshot(reportsSnapshot: CompanyReportsSnapshot): Map[String, BigDecimal] = {
    val quarterlyReportNumericAttributes = reportsSnapshot.quarterlyReport.map { quarterlyReport =>
      extractNumericAttributes(quarterlyReport.incomeStatement) ++
        extractNumericAttributes(quarterlyReport.balanceSheet) ++
        extractNumericAttributes(quarterlyReport.cashFlowStatement)
    }.getOrElse(Map[String, BigDecimal]())

    val eodAttributes = reportsSnapshot.eodBar.map(extractNumericAttributes(_)).getOrElse(Map[String, BigDecimal]())

    quarterlyReportNumericAttributes ++ eodAttributes
  }

  // queryForFutureReturn(123, 20050101120000, Seq(days(1), days(2), days(3)), 10000, 7, 0)
  // => Map("P1D" -> 1.01, "P2D" -> 1.02, "P3D" -> 0.97)
  def queryForFutureReturns(securityId: SecurityId,
                            baseTime: DateTime,
                            timeInFutureOffsets: Seq[Period],
                            principal: BigDecimal,
                            commissionPerTrade: BigDecimal,
                            commissionPerShare: BigDecimal)
                           (implicit adapter: Adapter): Map[String, BigDecimal] = {
    val pairs = for {
      period <- timeInFutureOffsets
      trialEndingInFuture <- adapter.queryForTrial(dke.tradesim.strategies.buyandhold.StrategyName,
                                                   securityId,
                                                   period,
                                                   date(baseTime),
                                                   principal,
                                                   commissionPerTrade,
                                                   commissionPerShare)
      futureReturn <- trialEndingInFuture.`yield`
    } yield (period.toString -> futureReturn)
    pairs.toMap
  }
}
