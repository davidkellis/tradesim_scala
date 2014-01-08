package dke.tradesim

import java.util.{NavigableMap, TreeMap}
import org.joda.time.DateTime
import net.sf.ehcache.{Element}

import dke.tradesim.core.{SecurityId, NumericAttribute, StatementAttribute, StatementType, Bar, FinancialReport, QuarterlyReport}
import dke.tradesim.datetimeUtils.{datetime, timestamp, seconds}
import dke.tradesim.db.{Adapter}
import dke.tradesim.logger._

import Adapter.dynamicAdapter

// IMPORTANT NOTE:
// The quarterly report logic in this module assumes that each quarterly report is published before the
// subsequent quarterly report; i.e. Q1report.publicationDate < Q2report.publicationDate
// A violation of this assumption will cause some of the logic in this module to break!

object quarterlyReports {
//  type FinancialReportHistory = NavigableMap[Long, FinancialReport]   // a price history is a collection of (timestamp -> FinancialReport) pairs
  type QuarterlyReportHistory = NavigableMap[Long, QuarterlyReport]   // a price history is a collection of (timestamp -> QuarterlyReport) pairs

  /**
   * Returns the most recent QuarterlyReport for <symbol> as of <date-time>.
   * If the given <date-time> falls within the interval of a particular report, then that report is returned;
   * If the given <date-time> does not fall within the interval of a particular report, then the most recent report as of that time is returned.
   *
   * Assumes that there is a mongodb collection named "quarterly_reports" containing the fields:
   *   s (ticker symbol),
   *   ts (timestamp representing the start of the interval that the report represents)
   *   te (timestamp representing the end of the interval that the report represents)
   * and that there is an ascending index of the form:
   *   index([
   *     [:s, 1],
   *     [:ts, 1]
   *   ],
   *   unique: true)
   */
  def queryQuarterlyReport(time: DateTime, securityId: SecurityId)(implicit adapter: Adapter): Option[QuarterlyReport] = {
    info(s"queryQuarterlyReport($time, $securityId)")
    adapter.queryQuarterlyReport(time, securityId)
  }

  /**
   * Returns the most recent QuarterlyReport for <symbol> occurring entirely before <date-time>.
   * Like query-quarterly-report, except that it returns the most recent report that ended before the given <date-time>.
   *
   * Assumes that there is a mongodb collection named "quarterly_reports" containing the fields:
   *   s (ticker symbol),
   *   ts (timestamp representing the start of the interval that the report represents)
   *   te (timestamp representing the end of the interval that the report represents)
   * and that there is an ascending index of the form:
   *   index([
   *     [:s, 1],
   *     [:te, 1]
   *   ],
   *   unique: true)
   */
  def queryQuarterlyReportPriorTo(time: DateTime, securityId: SecurityId)(implicit adapter: Adapter): Option[QuarterlyReport] = {
    info(s"queryQuarterlyReportPriorTo($time, $securityId)")
    adapter.queryQuarterlyReportPriorTo(time, securityId)
  }

  /**
   * Queries the database and returns an array of QuarterlyReport objects representing the quarterly report history of symbol <symbol>.
   * Example:
   *   (query-quarterly-reports "AAPL" (date-time 2001 1 1) (date-time 2001 12 31 23 59 59))
   */
  def queryQuarterlyReports(securityId: SecurityId)(implicit adapter: Adapter): Seq[QuarterlyReport] = {
    info(s"queryQuarterlyReports($securityId)")
    adapter.queryQuarterlyReports(securityId)
  }

  def queryQuarterlyReports(securityId: SecurityId, earliestTime: DateTime, latestTime: DateTime)(implicit adapter: Adapter): Seq[QuarterlyReport] = {
    info(s"queryQuarterlyReports($securityId, $earliestTime, $latestTime)")
    adapter.queryQuarterlyReports(securityId, earliestTime, latestTime)
  }

  /**
   * quarterly-report-history is a NavigableMap holding QuarterlyReports, sorted by publication-time of the report
   * timestamp is a Long timestamp of the form yyyymmddHHMMSS
   * quarterly-report-history is a java.util.NavigableMap of timestamp/report pairs.
   * returns a vector pair of the form [timestamp quote-record] if a report was found, or [nil nil] otherwise.
   */
  def mostRecentQuarterlyReport[ReportType](history: NavigableMap[Long, ReportType], timestamp: Long): Option[ReportType] = {
    val mapEntry = history.floorEntry(timestamp)
    Option(mapEntry).map(_.getValue)
  }

  /**
   * returns a util.NavigableMap holding the reports, sorted by the publication-time of the report
   * A NavigableMap doesn't need to be synchronized for concurrent reads - it is thread-safe for concurrent reads.
   * From the API docs:
   *   If multiple threads access a map concurrently, and at least one of the threads modifies the map structurally,
   *   it must be synchronized externally. (A structural modification is any operation that adds or deletes one or more mappings;
   *   merely changing the value associated with an existing key is not a structural modification.)
   */
  def loadQuarterlyReportHistoryFromReports(reports: Seq[QuarterlyReport]): QuarterlyReportHistory = {
    val quarterlyReportHistory: QuarterlyReportHistory = new TreeMap[Long, QuarterlyReport]()
    for {report <- reports} quarterlyReportHistory.put(timestamp(report.publicationTime), report)
    quarterlyReportHistory
  }

  /**
   * Returns a NavigableMap holding the quarterly reports (from the database) for <symbol> (optionally, between earliest-datetime and latest-datetime).
   * The map's keys are sorted by the report's start-time.
   */
  def loadQuarterlyReportHistory(securityId: SecurityId): QuarterlyReportHistory = loadQuarterlyReportHistoryFromReports(queryQuarterlyReports(securityId))
  def loadQuarterlyReportHistory(securityId: SecurityId, earliestTime: DateTime, latestTime: DateTime): QuarterlyReportHistory =
    loadQuarterlyReportHistoryFromReports(queryQuarterlyReports(securityId, earliestTime, latestTime))


  val quarterlyReportHistoryCache = cache.buildLruCache(32, "quarterlyReportHistoryCache")

  // loads up 2 years of quarterly reports
  def findQuarterlyReportHistory(year: Int, securityId: SecurityId): QuarterlyReportHistory = {
    val startYear = year - year % 2
    val quarterlyReportHistoryId = securityId.toString ++ ":" ++ startYear.toString
    val quarterlyReportHistory = Option(quarterlyReportHistoryCache.get(quarterlyReportHistoryId))
    quarterlyReportHistory match {
      case Some(quarterlyReportHistoryElement) => quarterlyReportHistoryElement.getObjectValue.asInstanceOf[QuarterlyReportHistory]
      case None =>
        val endYear = startYear + 1
        val newQuarterlyReportHistory = loadQuarterlyReportHistory(securityId,
                                                                   datetime(startYear, 1, 1),
                                                                   datetime(endYear, 12, 31, 23, 59, 59))    // load 2 calendar years of quarterly reports into a NavigableMap
        quarterlyReportHistoryCache.put(new Element(quarterlyReportHistoryId, newQuarterlyReportHistory))
        newQuarterlyReportHistory
    }
  }

  def mostRecentQuarterlyReportFromYear(time: DateTime, securityId: SecurityId, year: Int): Option[QuarterlyReport] = {
    val quarterlyReportHistory = findQuarterlyReportHistory(year, securityId)
    mostRecentQuarterlyReport(quarterlyReportHistory, timestamp(time))
  }

  // returns the most recent quarterly report for <securityId> as of <time>
  def findQuarterlyReport(time: DateTime, securityId: SecurityId): Option[QuarterlyReport] = {
    val year = time.getYear
    val quarterlyReport = mostRecentQuarterlyReportFromYear(time, securityId, year)               // search the 2-year period that <year> falls within
                          .orElse(mostRecentQuarterlyReportFromYear(time, securityId, year - 2))  // search the prior 2-year period immediately before the 2-year period that <year> falls within
    quarterlyReport.orElse(queryQuarterlyReport(time, securityId))
  }

  // returns a sequence of consecutive quarterly reports for <securityId> as of <time>, sorted in order of most recent (newest) to least recent (oldest) publication time
  def findQuarterlyReports(time: DateTime, securityId: SecurityId): Stream[QuarterlyReport] = {
    val mostRecentQuarterlyReport = findQuarterlyReport(time, securityId)
    mostRecentQuarterlyReport match {
      case Some(quarterlyReport) => quarterlyReport #:: findQuarterlyReports(quarterlyReport.publicationTime.minus(seconds(1)), securityId)
      case None => Stream.empty[QuarterlyReport]
    }
  }

  // returns a Some(Stream[QuarterlyReport] of length <qty>) or None
  def findQuarterlyReports(time: DateTime, securityId: SecurityId, qty: Int): Option[Stream[QuarterlyReport]] = {
    val reports = findQuarterlyReports(time: DateTime, securityId: SecurityId).take(qty)
    if (reports.length == qty) Some(reports)
    else None
  }

  def quarterlyReportAttribute(quarterlyReport: QuarterlyReport, statementType: StatementType.Value, attribute: String): Option[StatementAttribute] = {
    statementType match {
      case StatementType.BalanceSheet => quarterlyReport.balanceSheet.get(attribute)
      case StatementType.IncomeStatement => quarterlyReport.incomeStatement.get(attribute)
      case StatementType.CashFlowStatement => quarterlyReport.cashFlowStatement.get(attribute)
    }
  }

  def quarterlyAttribute(time: DateTime, securityId: SecurityId, statementType: StatementType.Value, attribute: String): Option[StatementAttribute] = {
    val quarterlyReport = findQuarterlyReport(time, securityId)
    quarterlyReport.flatMap(quarterlyReportAttribute(_, statementType, attribute))
  }

  def numericQuarterlyAttribute(quarterlyReport: QuarterlyReport, statementType: StatementType.Value, attribute: String): Option[BigDecimal] = {
    val attr = quarterlyReportAttribute(quarterlyReport, statementType, attribute)
    extractNumericAttribute(attr)
  }

  def numericQuarterlyAttribute(time: DateTime, securityId: SecurityId, statementType: StatementType.Value, attribute: String): Option[BigDecimal] = {
    val attr = quarterlyAttribute(time, securityId, statementType, attribute)
    extractNumericAttribute(attr)
  }

  def extractNumericAttribute(attribute: Option[StatementAttribute]): Option[BigDecimal] = attribute match {
    case Some(NumericAttribute(number)) => Option(number)
    case _ => None
  }

  // returns a sequence of attributes taken from consecutive quarterly reports
  def quarterlyAttributeSequence(time: DateTime, securityId: SecurityId, statementType: StatementType.Value, attribute: String): Stream[Option[StatementAttribute]] = {
    val quarterlyReports = findQuarterlyReports(time, securityId)
    quarterlyReports.map(quarterlyReportAttribute(_, statementType, attribute))
  }

  // returns a sequence of numeric attributes taken from consecutive quarterly reports
  def numericQuarterlyAttributes(time: DateTime, securityId: SecurityId, statementType: StatementType.Value, attribute: String): Stream[Option[BigDecimal]] = {
    val attrs = quarterlyAttributeSequence(time, securityId, statementType, attribute)
    attrs.map(extractNumericAttribute)
  }

  // use for computing growth from Q1 2005 to Q2 2005
  def quarterOverQuarterGrowth(time: DateTime,
                               securityId: SecurityId,
                               statementType: StatementType.Value,
                               attribute: String): Option[BigDecimal] = {
    numericQuarterlyAttributes(time, securityId, statementType, attribute).take(2) match {
      case Stream(Some(newestAttr), Some(olderAttr)) => Some(newestAttr / olderAttr - 1.0)
      case _ => None
    }
  }

  // use for computing growth from Q1 2004 to Q1 2005
  def quarterOnQuarterGrowth(time: DateTime,
                             securityId: SecurityId,
                             statementType: StatementType.Value,
                             attribute: String): Option[BigDecimal] = {
    numericQuarterlyAttributes(time, securityId, statementType, attribute).take(5) match {
      case Stream(Some(newestAttr), _, _, _, Some(olderAttr)) => Some(newestAttr / olderAttr - 1.0)
      case _ => None
    }
  }

}