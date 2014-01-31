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
  case class CompanyReportsSnapshot(time: DateTime, quarterlyReport: Option[QuarterlyReport], eodBar: Option[Bar])
  case class AttributeSetSnapshot(time: DateTime, quarterlyReportAttributes: Map[String, BigDecimal], eodBarAttributes: Map[String, BigDecimal], futureReturns: Map[String, BigDecimal])

  def buildAttributesTable(attributeSetSnapshots: Seq[AttributeSetSnapshot], headersRow: Seq[String] = Seq[String](), tableData: Seq[Seq[String]] = Seq[Seq[String]]()): Seq[Seq[String]] = {
    attributeSetSnapshots.headOption match {
      case None =>
        tableData.+:(headersRow)
      case Some(attributeSetSnapshot) =>
        val headers = (attributeSetSnapshot.eodBarAttributes.keys ++
          attributeSetSnapshot.quarterlyReportAttributes.keys ++
          attributeSetSnapshot.futureReturns.keys).toSeq
        val missingHeaders = headers.diff(headers)
        val newHeadersRow = headersRow ++ missingHeaders
        val newRow = Seq[String]()
        val newTableData = tableData :+ newRow
        buildAttributesTable(attributeSetSnapshots.tail, newHeadersRow, newTableData)
    }
  }

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
    val quarterlyReportAttributes = reportsSnapshot.quarterlyReport.map(extractNumericAttributesFromReportSnapshot(_)).getOrElse(Map[String, BigDecimal]())
    val eodBarAttributes = reportsSnapshot.eodBar.map(extractNumericAttributes(_)).getOrElse(Map[String, BigDecimal]())

    val futureReturns = queryForFutureReturns(securityId, time, timeInFutureOffsets, principal, commissionPerTrade, commissionPerShare)

    AttributeSetSnapshot(time, quarterlyReportAttributes, eodBarAttributes, futureReturns)
  }

  // companyReportsSnapshot(datetime(2005, 1, 1), 123)
  // => CompanyReportsSnapshot(<DateTime@20050101000000>, Some(<QuarterlyReport object>), Some(<EodBar object>))
  def companyReportsSnapshot(time: DateTime, securityId: SecurityId): CompanyReportsSnapshot = {
    CompanyReportsSnapshot(
      time,
      quarterlyReports.findQuarterlyReport(time, securityId),
      quotes.findEodBar(time, securityId)
    )
  }

  // extractNumericAttributesFromReportsSnapshot(companyReportsSnapshot(datetime(2005, 1, 1), 123))
  // => Map("Revenue" -> BigDecimal(50000000), "Cost of goods sold" -> BigDecimal(35000000), ...)
  def extractNumericAttributesFromReportSnapshot(quarterlyReport: QuarterlyReport): Map[String, BigDecimal] = {
    extractNumericAttributes(quarterlyReport.incomeStatement) ++
      extractNumericAttributes(quarterlyReport.balanceSheet) ++
      extractNumericAttributes(quarterlyReport.cashFlowStatement)
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
