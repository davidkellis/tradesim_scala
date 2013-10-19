package dke.tradesim

import java.util.{NavigableMap, TreeMap}
import org.joda.time.DateTime
import net.sf.ehcache.{Element}

import dke.tradesim.core.{Bar}
import dke.tradesim.datetimeUtils.{timestamp, datetime, isInstantBetweenInclusive, millis}
import dke.tradesim.db.{Adapter}
import dke.tradesim.logger._

import Adapter.threadLocalAdapter

object quotes {
  def barOpen(bar: Bar): BigDecimal = bar.open
  def barHigh(bar: Bar): BigDecimal = bar.high
  def barLow(bar: Bar): BigDecimal = bar.low
  def barClose(bar: Bar): BigDecimal = bar.close

  val barSimQuoteCache = cache.buildLruCache(200, "barSimQuote")    // this cache holds String/BigDecimal pairs
  def barSimQuote(bar: Bar): BigDecimal = {
    val barId = bar.securityId.toString.concat(bar.startTime.toString("yyyyMMddHHmmss"))
    val cachedQuote = Option(barSimQuoteCache.get(barId))
    cachedQuote match {
      case Some(priceQuoteElement) => priceQuoteElement.getObjectValue.asInstanceOf[BigDecimal]
      case None =>
        val newQuote = (bar.low + bar.high) / BigDecimal(2)
        barSimQuoteCache.put(new Element(barId, newQuote))
        newQuote
    }
  }
//  def barSimQuote(bar: Bar): BigDecimal = (bar.low + bar.high) / BigDecimal(2)

  /**
   * Returns the most recent EOD bar for <symbol> as of <date-time>.
   * If the given <date-time> falls within the interval of a particular bar, then that bar is returned;
   * If the given <date-time> does not fall within the interval of a particular bar, then the most recent bar as of that time is returned.
   * The bar returned is not adjusted for splits or dividend payments.
   *
   * Assumes that there is a mongodb collection named "eods" containing the fields:
   *   s (ticker symbol),
   *   ts (timestamp representing the start of the interval that the bar represents)
   *   te (timestamp representing the end of the interval that the bar represents)
   * and that there is an ascending index of the form:
   *   index([
   *     [:s, 1],
   *     [:ts, 1]
   *   ],
   *   unique: true)
   */
  def queryEodBar(time: DateTime, securityId: SecurityId)(implicit adapter: Adapter): Option[Bar] = {
    info(s"queryEodBar($time, $securityId)")
    adapter.queryEodBar(time, securityId)
  }

  /**
   * Returns the most recent EOD bar for <symbol> occurring entirely before <date-time>.
   * Like query-eod-bar, except that it returns the most recent EOD bar that ended before the given <date-time>.
   *
   * Assumes that there is a mongodb collection named "eods" containing the fields:
   *   s (ticker symbol),
   *   ts (timestamp representing the start of the interval that the bar represents)
   *   te (timestamp representing the end of the interval that the bar represents)
   * and that there is an ascending index of the form:
   *   index([
   *     [:s, 1],
   *     [:te, 1]
   *   ],
   *   unique: true)
   */
  def queryEodBarPriorTo(time: DateTime, securityId: SecurityId)(implicit adapter: Adapter): Option[Bar] = {
    info(s"queryEodBarPriorTo($time, $securityId)")
    adapter.queryEodBarPriorTo(time, securityId)
  }

  def queryEodBars(securityId: SecurityId)(implicit adapter: Adapter): Seq[Bar] = {
    info(s"queryEodBars($securityId)")
    adapter.queryEodBars(securityId)
  }

  def queryEodBars(securityId: SecurityId, earliestTime: DateTime, latestTime: DateTime)(implicit adapter: Adapter): Seq[Bar] = {
    info(s"queryEodBars($securityId, $earliestTime, $latestTime)")
    var t1 = datetimeUtils.currentTime()
    val result = adapter.queryEodBars(securityId, earliestTime, latestTime)
    var t2 = datetimeUtils.currentTime()
    verbose(s"Time: ${datetimeUtils.prettyFormatPeriod(datetimeUtils.periodBetween(t1, t2))}")
    result
  }


  type PriceHistory = NavigableMap[Long, Bar]   // a price history is a collection of (timestamp -> Bar) pairs

  def loadPriceHistoryFromBars(bars: Seq[Bar]): PriceHistory = {
    val priceHistory: PriceHistory = new TreeMap[Long, Bar]()
    for {bar <- bars} priceHistory.put(timestamp(bar.startTime), bar)
    priceHistory
  }

  def loadPriceHistory(securityId: SecurityId): PriceHistory = loadPriceHistoryFromBars(queryEodBars(securityId))
  def loadPriceHistory(securityId: SecurityId, earliestTime: DateTime, latestTime: DateTime): PriceHistory =
    loadPriceHistoryFromBars(queryEodBars(securityId, earliestTime, latestTime))

  def mostRecentBar(priceHistory: PriceHistory, timestamp: Long): Option[Bar] = {
    val mapEntry = priceHistory.floorEntry(timestamp)
    Option(mapEntry).map(_.getValue)
  }

  def mostRecentBarFromYear(time: DateTime, securityId: SecurityId, year: Int): Option[Bar] = {
    val priceHistory = findPriceHistory(year, securityId)
    mostRecentBar(priceHistory, timestamp(time))
  }

  def findEodBar(time: DateTime, securityId: SecurityId): Option[Bar] = {
    val year = time.getYear
    val bar: Option[Bar] = mostRecentBarFromYear(time, securityId, year).orElse(mostRecentBarFromYear(time, securityId, year - 1))
    bar.orElse(queryEodBar(time, securityId))
  }

  def findEodBarPriorTo(time: DateTime, securityId: SecurityId): Option[Bar] = {
    val eodBar = findEodBar(time, securityId)
    eodBar.flatMap { bar =>
      if (isInstantBetweenInclusive(time, bar.startTime, bar.endTime))
        findEodBar(bar.startTime.minus(millis(1)), securityId)
      else Option(bar)
    }
  }

  def findOldestEodBar(securityId: SecurityId)(implicit adapter: Adapter): Option[Bar] = {
    info(s"findOldestEodBar($securityId)")
    adapter.findOldestEodBar(securityId)
  }

  def findMostRecentEodBar(securityId: SecurityId)(implicit adapter: Adapter): Option[Bar] = {
    info(s"findMostRecentEodBar($securityId)")
    adapter.findMostRecentEodBar(securityId)
  }


  val priceHistoryCache = cache.buildLruCache(32, "priceHistoryCache")

  // loads up 5 years of price history
  def findPriceHistory(year: Int, securityId: SecurityId): PriceHistory = {
    val startYear = year - year % 5
    val priceHistoryId = securityId.toString ++ ":" ++ startYear.toString
    val cachedPriceHistory = Option(priceHistoryCache.get(priceHistoryId))
    cachedPriceHistory match {
      case Some(priceHistoryElement) => priceHistoryElement.getObjectValue.asInstanceOf[PriceHistory]
      case None =>
        val endYear = startYear + 4
        val newPriceHistory = loadPriceHistory(securityId,
                                               datetime(startYear, 1, 1),
                                               datetime(endYear, 12, 31, 23, 59, 59))    // load 5 calendar years of price history into a NavigableMap
        priceHistoryCache.put(new Element(priceHistoryId, newPriceHistory))
        newPriceHistory
    }
  }
}