package dke.tradesim

import org.joda.time.{Period, Interval, DateTime}
import dke.tradesim.datetimeUtils.{intervalBetween, isAfter, isBefore, minDateTime, maxDateTime, offsetInterval}
import dke.tradesim.quotes.{findOldestEodBar, findMostRecentEodBar}
import dke.tradesim.core.SecurityId

object priceHistory {
  // Returns the interval of time that spans the full price history of a particular symbol
  def priceHistoryInterval(securityId: SecurityId): Option[Interval] = {
    val startTime = findOldestEodBar(securityId).map(_.startTime)
    val endTime = findMostRecentEodBar(securityId).map(_.endTime)
    if (startTime.isDefined && endTime.isDefined) Option(intervalBetween(startTime.get, endTime.get)) else None
  }

  def priceHistoryContains(securityId: SecurityId, interval: Interval): Boolean =
    priceHistoryInterval(securityId).map(_.contains(interval)).getOrElse(false)

  def isEnoughPriceHistory(securityId: SecurityId, tradingPeriodLength: Period): Boolean = {
    val interval = priceHistoryInterval(securityId)
    if (interval.isDefined) {
      val (start, end) = interval.map(i => (i.getStart, i.getEnd)).get
      val tradingStart = end.minus(tradingPeriodLength)
      !isAfter(start, tradingStart)
    } else false
  }

  def securitiesWithEnoughPriceHistory(securityIds: Seq[Int], tradingPeriodLength: Period): Seq[Int] =
    securityIds.filter(isEnoughPriceHistory(_, tradingPeriodLength))

  /**
   * Returns the earliest and latest start-of-trading-period datetimes that the ticker represented by symbol may be traded,
   * given that an experiment may last for up to trading-period-length.
   * Usage: (trading-period-start-dates "intraday_data/AAPL.csv" (org.joda.time.Period/years 1))
   *        -> [#<DateTime 1999-04-01T08:32:00.000-06:00> #<DateTime 2008-04-01T13:42:00.000-05:00>]
   * For Reference: (price-history-start-end "intraday_data/AAPL.csv")
   *                -> [#<DateTime 1999-04-01T08:32:00.000-06:00> #<DateTime 2009-04-01T13:42:00.000-05:00>]
   */
  def tradingPeriodStartDates(securityId: SecurityId, tradingPeriodLength: Period): Option[(DateTime, DateTime)] = {
    priceHistoryInterval(securityId).flatMap {interval =>
      val (start, end) = (interval.getStart, interval.getEnd)
      val adjustedEnd = end.minus(tradingPeriodLength)
      if (isBefore(adjustedEnd, start)) None
      else Option( (start, adjustedEnd) )
    }
  }

  /**
   * Returns the common date range (CDR) of a set of price histories.
   * Example, we have the following price history for each of 3 companies.
   * Company A:                            |----------------------------------------------------------------------------|
   * Company B:                         |------------------------------------------------|
   * Company C:                                          |-------------------------------------------------------|
   * CDR (common date range):                            |-------------------------------|
   *
   * Returns an interval representing the start and end of the CDR.
   * If there is no common overlap among the companies, then the function returns nil.
   *
   * Example: (common-price-history-date-range ["AAPL", "F", "VFINX"])
   *          -> [#<DateTime 1987-03-27T09:30:00.000Z> #<DateTime 2012-10-10T16:00:00.000Z>]
   */
  def commonPriceHistoryDateRange(securityIds: Seq[Int]): Option[Interval] = {
    val intervals = securityIds.map(priceHistoryInterval(_))
    val start = intervals.flatMap(intervalOp => intervalOp.map(interval => interval.getStart)).reduceLeft(maxDateTime)  // get the latest (max) start date
    val end = intervals.flatMap(intervalOp => intervalOp.map(interval => interval.getEnd)).reduceLeft(minDateTime)      // get the earliest (min) end date
    if (isBefore(end, start)) None
    else Option(intervalBetween(start, end))
  }

  /**
   * Returns a pair of datetimes representing the earliest and latest dates that a trading strategy
   * may begin simultaneously trading a group of companies, assuming the trading strategy *may* trade the companies
   * for up to trading-period-length.
   *
   * Example, we have the following price history for each of 3 companies.
   * Company A:                            |----------------------------------------------------------------------------|
   * Company B:                         |------------------------------------------------|
   * Company C:                                          |-------------------------------------------------------|
   * CDR (common date range):                            |-------------------------------|
   * So, since the CDR (common date range) is the time period that we have price history information for all 3 companies
   * we can trade all 3 companies simultaneously during that time period ONLY if the time period is at least as long as
   * trading-period-length.
   *
   * This function returns a pair representing the earliest and latest start-of-trading-period datetimes that all companies
   * can be traded simultaneously for a period of trading-period-length.
   *
   * Usage: (common-trial-period-start-dates ["AAPL" "F"] (years 1))
   *        -> [#<DateTime 1984-09-07T09:30:00.000Z> #<DateTime 2011-10-10T16:00:00.000Z>]
   */
  def commonTrialPeriodStartDates(securityIds: Seq[Int], trialPeriodLength: Period): Option[Interval] = {
    val intervals = securityIds.map(priceHistoryInterval(_))
    val start = intervals.flatMap(intervalOp => intervalOp.map(interval => interval.getStart)).reduceLeft(maxDateTime)  // get the latest (max) start date
    val end = intervals.flatMap(intervalOp => intervalOp.map(interval => interval.getEnd)).reduceLeft(minDateTime)      // get the earliest (min) end date
    val adjustedEnd = end.minus(trialPeriodLength)
    if (isBefore(adjustedEnd, start)) None
    else Option(intervalBetween(start, adjustedEnd))
  }
  def commonTrialPeriodStartDates(securityIds: Seq[Int],
                                  trialPeriodLength: Period,
                                  startOffsetDirection: Symbol,
                                  startOffset: Period,
                                  endOffsetDirection: Symbol,
                                  endOffset: Period): Option[Interval] = {
    val offsetPriceHistoryInterval: (SecurityId) => Option[Interval] =
      priceHistoryInterval(_).map(offsetInterval(_, startOffsetDirection, startOffset, endOffsetDirection, endOffset))
    val intervals = securityIds.map(offsetPriceHistoryInterval)
    val start = intervals.flatMap(intervalOp => intervalOp.map(interval => interval.getStart)).reduceLeft(maxDateTime)  // get the latest (max) start date
    val end = intervals.flatMap(intervalOp => intervalOp.map(interval => interval.getEnd)).reduceLeft(minDateTime)      // get the earliest (min) end date
    val adjustedEnd = end.minus(trialPeriodLength)
    if (isBefore(adjustedEnd, start)) None
    else Option(intervalBetween(start, adjustedEnd))
  }
}