package dke.tradesim.analyses

import org.joda.time.{Period, LocalDate, DateTime}

import dke.tradesim.core._
import dke.tradesim.datetimeUtils.{date, datetime, interspersedDateSeries, days}
import dke.tradesim.{quotes, quarterlyReports}
import dke.tradesim.priceHistory.{priceHistoryInterval}
import dke.tradesim.schedule.{defaultHolidaySchedule, defaultTradingSchedule, buildTradingSchedule, isTradingDay, TradingSchedule}
import dke.tradesim.core.QuarterlyReport
import dke.tradesim.database.Tables._

object predictiveVariables {
  case class CompanySnapshot(quarterlyReport: Option[QuarterlyReport], eodBar: Option[Bar])
  case class AttributeSetSnapshot(attributes: Map[String, BigDecimal], futureReturns: Map[String, BigDecimal])

  def attributeSetSnapshotSeries(securityId: SecurityId, tradingSchedule: TradingSchedule, timeOffsets: Seq[Period]): Stream[AttributeSetSnapshot] = {
    tradingDaysSpanningPriceHistory(securityId, tradingSchedule).map(date => attributeSetSnapshot(securityId, datetime(date, 12, 0, 0), timeOffsets))
  }

  def tradingDaysSpanningPriceHistory(securityId: SecurityId, tradingSchedule: TradingSchedule): Stream[LocalDate] = {
    val securityPriceHistoryInterval = priceHistoryInterval(securityId)
    securityPriceHistoryInterval.map { interval =>
      interspersedDateSeries(date(interval.getStart), date(interval.getEnd), days(1)).filter(date => isTradingDay(date, tradingSchedule))
    }.getOrElse(Stream.empty[LocalDate])
  }

//  def attributeSeries(securityId: SecurityId,
//                      tradingSchedule: TradingSchedule,
//                      attributeGetterFn: (DateTime, SecurityId) => BigDecimal): Stream[BigDecimal] = {
//    tradingDaysSpanningPriceHistory(securityId, tradingSchedule).map(date => attributeGetterFn(datetime(date), securityId))
//  }

  def attributeSetSnapshot(securityId: SecurityId, time: DateTime, timeOffsets: Seq[Period]): AttributeSetSnapshot = {
    val snapshot = companySnapshot(time, securityId)
    val quarterlyReportNumericAttributes = snapshot.quarterlyReport.map { quarterlyReport =>
      extractNumericAttributes(quarterlyReport.incomeStatement) ++
      extractNumericAttributes(quarterlyReport.balanceSheet) ++
      extractNumericAttributes(quarterlyReport.cashFlowStatement)
    }.getOrElse(Map[String, BigDecimal]())
    val eodAttributes = snapshot.eodBar.map(extractNumericAttributes(_)).getOrElse(Map[String, BigDecimal]())
    val attributes = quarterlyReportNumericAttributes ++ eodAttributes

    val futureReturns = queryForFutureReturn(securityId, time, timeOffsets)

    AttributeSetSnapshot(attributes, futureReturns)
  }

  def companySnapshot(time: DateTime, securityId: SecurityId): CompanySnapshot = {
    CompanySnapshot(
      quarterlyReports.findQuarterlyReport(time, securityId),
      quotes.findEodBar(time, securityId)
    )
  }

  def queryForFutureReturn(securityId: SecurityId, baseTime: DateTime, timeInFutureOffsets: Seq[Period], principal: BigDecimal, commissionPerTrade: BigDecimal, commissionPerShare: BigDecimal): Map[String, BigDecimal] = {
    timeInFutureOffsets.flatMap { period =>
      val trialEndingInFuture = queryForTrial(dke.tradesim.strategies.buyandhold.StrategyName, securityId, period, date(baseTime), principal, commissionPerTrade, commissionPerShare)
      val futureReturn = trialEndingInFuture.`yield`
      futureReturn.map(period.toString -> _)
    }.toMap
  }

  def queryForTrial(strategyName: String, securityId: SecurityId, trialDuration: Period, startDate: LocalDate, principal: BigDecimal, commissionPerTrade: BigDecimal, commissionPerShare: BigDecimal): TrialsRow = {
    (for {
      strategy <- Strategies if strategy.name === strategyName
      ts <- TrialSets if ts.strategyId === strategy.id
      sToTs <- SecuritiesTrialSets if sToTs.trialSetId === ts.id
      s <- Securities if s.id === sToTs.securityId
      if s.id === securityId
      if ts.principal === principal
      if ts.commissionPerTrade === commissionPerTrade
      if ts.commissionPerShare === commissionPerShare
    } yield (ts.id, s.id)).list

    val trials = Trials.filter(_.strategy_.securityId === securityId).filter(_.endTime < timestamp(time))
    val sortedReports = reports.sortBy(_.endTime.desc)
    val record = sortedReports.take(1).firstOption

  }
}
