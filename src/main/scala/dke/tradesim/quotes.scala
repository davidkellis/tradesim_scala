package dke.tradesim

import java.util.{NavigableMap, TreeMap}
import org.joda.time.DateTime
import dke.tradesim.core.Bar
import dke.tradesim.datetime.{timestamp, datetime, isInstantBetweenInclusive, millis}
import dke.tradesim.db.EodBars

import scala.slick.driver.PostgresDriver.simple._
import net.sf.ehcache.{Element, CacheManager}

object quotes {
  def barOpen(bar: Bar): BigDecimal = bar.open
  def barHigh(bar: Bar): BigDecimal = bar.high
  def barLow(bar: Bar): BigDecimal = bar.low
  def barClose(bar: Bar): BigDecimal = bar.close
  def barSimQuote(bar: Bar): BigDecimal = (bar.low + bar.high) / 2

  def convertEodDbRecord(): Bar = ???

  def queryEodBar(time: DateTime, symbol: String): Option[Bar] = {
    val bars = Query(EodBars).filter(_.symbol == symbol).filter(_.startTime <= timestamp(time)).sortBy(_.startTime).desc
    ???
  }

  def queryEodBarPriorTo(time: DateTime, symbol: String): Option[Bar] = ???

  def queryEodBars(symbol: String): Seq[Bar] = ???
  def queryEodBars(symbol: String, earliestTime: DateTime, latestTime: DateTime): Seq[Bar] = ???


  type PriceHistory = NavigableMap[String, Bar]

  def loadPriceHistoryFromBars(bars: Seq[Bar]): PriceHistory = {
    val priceHistory: PriceHistory = new TreeMap[String, Bar]()
    for {bar <- bars} priceHistory.put(timestamp(bar.startTime), bar)
    priceHistory
  }

  def loadPriceHistory(symbol: String): PriceHistory = loadPriceHistoryFromBars(queryEodBars(symbol))
  def loadPriceHistory(symbol: String, earliestTime: DateTime, latestTime: DateTime): PriceHistory =
    loadPriceHistoryFromBars(queryEodBars(symbol, earliestTime, latestTime))

  def mostRecentBar(priceHistory: PriceHistory, timestamp: String): Option[Bar] = {
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
      case Some(priceHistoryElement) => priceHistoryElement.getObjectValue().asInstanceOf[PriceHistory]
      case None =>
        val newPriceHistory = loadPriceHistory(symbol, datetime(year, 1, 1), datetime(year, 12, 31, 23, 59, 59))    // load one calendar year of price history into a NavigableMap
        priceHistoryCache.put(new Element(priceHistoryId, newPriceHistory))
        newPriceHistory
    }
  }
}