package dke.tradesim

import java.util.{NavigableMap, TreeMap}
import org.joda.time.DateTime
import net.sf.ehcache.{Element}

import dke.tradesim.core.{NumericAttribute, StatementAttribute, StatementType, Bar, FinancialReport, QuarterlyReport}
import dke.tradesim.datetimeUtils.{datetime, timestamp}
import dke.tradesim.db.{Adapter}
import dke.tradesim.logger._

import Adapter.threadLocalAdapter


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
  def queryQuarterlyReport(time: DateTime, symbol: String)(implicit adapter: Adapter): Option[QuarterlyReport] = {
    info(s"queryQuarterlyReport($time, $symbol)")
    adapter.queryQuarterlyReport(time, symbol)
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
  def queryQuarterlyReportPriorTo(time: DateTime, symbol: String)(implicit adapter: Adapter): Option[QuarterlyReport] = {
    info(s"queryQuarterlyReportPriorTo($time, $symbol)")
    adapter.queryQuarterlyReportPriorTo(time, symbol)
  }

  /**
   * Queries the database and returns an array of QuarterlyReport objects representing the quarterly report history of symbol <symbol>.
   * Example:
   *   (query-quarterly-reports "AAPL" (date-time 2001 1 1) (date-time 2001 12 31 23 59 59))
   */
  def queryQuarterlyReports(symbol: String)(implicit adapter: Adapter): Seq[QuarterlyReport] = {
    info(s"queryQuarterlyReports($symbol)")
    adapter.queryQuarterlyReports(symbol)
  }

  def queryQuarterlyReports(symbol: String, earliestTime: DateTime, latestTime: DateTime)(implicit adapter: Adapter): Seq[QuarterlyReport] = {
    info(s"queryQuarterlyReports($symbol, $earliestTime, $latestTime)")
    adapter.queryQuarterlyReports(symbol, earliestTime, latestTime)
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
  def loadQuarterlyReportHistory(symbol: String): QuarterlyReportHistory = loadQuarterlyReportHistoryFromReports(queryQuarterlyReports(symbol))
  def loadQuarterlyReportHistory(symbol: String, earliestTime: DateTime, latestTime: DateTime): QuarterlyReportHistory =
    loadQuarterlyReportHistoryFromReports(queryQuarterlyReports(symbol, earliestTime, latestTime))


  val quarterlyReportHistoryCache = cache.buildLruCache(32, "quarterlyReportHistoryCache")

  // loads up 2 years of quarterly reports
  def findQuarterlyReportHistory(year: Int, symbol: String): QuarterlyReportHistory = {
    val startYear = year - year % 2
    val quarterlyReportHistoryId = symbol ++ ":" ++ startYear.toString
    val quarterlyReportHistory = Option(quarterlyReportHistoryCache.get(quarterlyReportHistoryId))
    quarterlyReportHistory match {
      case Some(quarterlyReportHistoryElement) => quarterlyReportHistoryElement.getObjectValue.asInstanceOf[QuarterlyReportHistory]
      case None =>
        val endYear = startYear + 1
        val newQuarterlyReportHistory = loadQuarterlyReportHistory(symbol,
                                                                   datetime(startYear, 1, 1),
                                                                   datetime(endYear, 12, 31, 23, 59, 59))    // load 2 calendar years of quarterly reports into a NavigableMap
        quarterlyReportHistoryCache.put(new Element(quarterlyReportHistoryId, newQuarterlyReportHistory))
        newQuarterlyReportHistory
    }
  }

  def mostRecentQuarterlyReportFromYear(time: DateTime, symbol: String, year: Int): Option[QuarterlyReport] = {
    val quarterlyReportHistory = findQuarterlyReportHistory(year, symbol)
    mostRecentQuarterlyReport(quarterlyReportHistory, timestamp(time))
  }

  // returns the most recent quarterly report for <symbol> as of <time>
  def findQuarterlyReport(time: DateTime, symbol: String): Option[QuarterlyReport] = {
    val year = time.getYear
    val quarterlyReport = mostRecentQuarterlyReportFromYear(time, symbol, year)               // search the 2-year period that <year> falls within
                          .orElse(mostRecentQuarterlyReportFromYear(time, symbol, year - 2))  // search the prior 2-year period immediately before the 2-year period that <year> falls within
    quarterlyReport.orElse(queryQuarterlyReport(time, symbol))
  }

  def quarterlyReportAttribute(time: DateTime, symbol: String, reportType: StatementType.Value, attribute: String): Option[StatementAttribute] = {
    val quarterlyReport = findQuarterlyReport(time, symbol)
    quarterlyReport.flatMap { quarterlyReport =>
      reportType match {
        case StatementType.BalanceSheet => quarterlyReport.balanceSheet.get(attribute)
        case StatementType.IncomeStatement => quarterlyReport.incomeStatement.get(attribute)
        case StatementType.CashFlowStatement => quarterlyReport.cashFlowStatement.get(attribute)
      }
    }
  }

  def numericQuarterlyReportAttribute(time: DateTime, symbol: String, report: StatementType.Value, attribute: String): Option[BigDecimal] = {
    val sharesOut = quarterlyReportAttribute(time, symbol, report, attribute)
    sharesOut match {
      case Some(NumericAttribute(shareCount)) => Option(shareCount)
      case _ => None
    }
  }

}