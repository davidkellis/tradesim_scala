package dke.tradesim

import java.util.{NavigableMap, TreeMap}
import org.joda.time.DateTime
import dke.tradesim.core.{Bar}
import dke.tradesim.datetimeUtils.{timestamp, datetime, isInstantBetweenInclusive, millis}
import dke.tradesim.db.{EodBars, convertEodBarRecord}

import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession
import net.sf.ehcache.{Element, CacheManager}

object quotes {
  def barOpen(bar: Bar): BigDecimal = bar.open
  def barHigh(bar: Bar): BigDecimal = bar.high
  def barLow(bar: Bar): BigDecimal = bar.low
  def barClose(bar: Bar): BigDecimal = bar.close
  def barSimQuote(bar: Bar): BigDecimal = (bar.low + bar.high) / 2

  def convertEodDbRecord(): Bar = ???

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
  def queryEodBar(time: DateTime, symbol: String): Option[Bar] = {
    val bars = Query(EodBars).filter(_.symbol === symbol).filter(_.startTime <= timestamp(time))
    val sortedBars = bars.sortBy(_.startTime.desc)
    val record = sortedBars.take(1).firstOption
    convertEodBarRecord(record)
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
  def queryEodBarPriorTo(time: DateTime, symbol: String): Option[Bar] = ???

  def queryEodBars(symbol: String): Seq[Bar] = ???
  def queryEodBars(symbol: String, earliestTime: DateTime, latestTime: DateTime): Seq[Bar] = ???


  type PriceHistory = NavigableMap[Long, Bar]   // a price history is a collection of (timestamp -> Bar) pairs

  def loadPriceHistoryFromBars(bars: Seq[Bar]): PriceHistory = {
    val priceHistory: PriceHistory = new TreeMap[Long, Bar]()
    for {bar <- bars} priceHistory.put(timestamp(bar.startTime), bar)
    priceHistory
  }

  def loadPriceHistory(symbol: String): PriceHistory = loadPriceHistoryFromBars(queryEodBars(symbol))
  def loadPriceHistory(symbol: String, earliestTime: DateTime, latestTime: DateTime): PriceHistory =
    loadPriceHistoryFromBars(queryEodBars(symbol, earliestTime, latestTime))

  def mostRecentBar(priceHistory: PriceHistory, timestamp: Long): Option[Bar] = {
    val mapEntry = priceHistory.floorEntry(timestamp)
    Option(mapEntry.getValue)
  }

  def mostRecentBarFromYear(time: DateTime, symbol: String, year: Int): Option[Bar] = {
    val priceHistory = findPriceHistory(year, symbol)
    mostRecentBar(priceHistory, timestamp(time))
  }

  def findEodBar(time: DateTime, symbol: String): Option[Bar] = {
    val year = time.getYear
    val bar: Option[Bar] = mostRecentBarFromYear(time, symbol, year).orElse(mostRecentBarFromYear(time, symbol, year - 1))
    bar.orElse(queryEodBar(time, symbol))
  }

  def findEodBarPriorTo(time: DateTime, symbol: String): Option[Bar] = {
    val eodBar = findEodBar(time, symbol)
    eodBar.flatMap { bar =>
      if (isInstantBetweenInclusive(time, bar.startTime, bar.endTime))
        findEodBar(bar.startTime.minus(millis(1)), symbol)
      else Option(bar)
    }
  }

  def findOldestEodBar(symbol: String): Option[Bar] = ???

  def findMostRecentEodBar(symbol: String): Option[Bar] = ???


  val priceHistoryCache = CacheManager.getInstance().getCache("priceHistoryCache")

  def findPriceHistory(year: Int, symbol: String): PriceHistory = {
    val priceHistoryId = symbol ++ ":" ++ year.toString
    val cachedPriceHistory = Option(priceHistoryCache.get(priceHistoryId))
    cachedPriceHistory match {
      case Some(priceHistoryElement) => priceHistoryElement.getObjectValue.asInstanceOf[PriceHistory]
      case None =>
        val newPriceHistory = loadPriceHistory(symbol, datetime(year, 1, 1), datetime(year, 12, 31, 23, 59, 59))    // load one calendar year of price history into a NavigableMap
        priceHistoryCache.put(new Element(priceHistoryId, newPriceHistory))
        newPriceHistory
    }
  }
}