package dke.tradesim

import org.joda.time._
import dke.tradesim.datetime.{dayOfWeek, intervalBetween, isHoliday, HolidayLookupFunctions, interspersedTimeSeries, isBeforeOrEqual}
import dke.tradesim.minterval.{MInterval, emptyMInterval, createMInterval, overlaps, subtractMInterval, isEmpty}

object schedule {
  val defaultStartOfTrading = new LocalTime(8, 30, 0)   // Eastern Time
  val defaultEndOfTrading = new LocalTime(15, 0, 0)     // Eastern Time
  val defaultDailyTradingHours = (defaultStartOfTrading, defaultEndOfTrading)

  val defaultWeeklyTradingHours = Map(
    DateTimeConstants.MONDAY -> defaultDailyTradingHours,
    DateTimeConstants.TUESDAY -> defaultDailyTradingHours,
    DateTimeConstants.WEDNESDAY -> defaultDailyTradingHours,
    DateTimeConstants.THURSDAY -> defaultDailyTradingHours,
    DateTimeConstants.FRIDAY -> defaultDailyTradingHours)

  def defaultTradingSchedule(time: DateTime): MInterval = {
    val tradingHours = defaultWeeklyTradingHours.get(dayOfWeek(time))
    tradingHours.map { tradingHours =>
      createMInterval(Vector(intervalBetween(tradingHours._1.toDateTime(time), tradingHours._2.toDateTime(time))))
    }.getOrElse(emptyMInterval)
  }

  // returns an MInterval spanning the time of the holiday - this MInterval represents the time we take off for the holiday
  def defaultHolidaySchedule(time: DateTime): MInterval = {
    if (HolidayLookupFunctions.exists(holidayLookupFn => isHoliday(time, holidayLookupFn)))
      defaultTradingSchedule(time)
    else
      emptyMInterval
  }

  type TradingSchedule = (LocalDate) => MInterval

  def buildTradingSchedule(normalTradingSchedule: TradingSchedule, holidaySchedule: TradingSchedule): TradingSchedule =
    (date: LocalDate) => {
      val tradingHours = normalTradingSchedule(date)
      val holidayHours = holidaySchedule(date)
      if (overlaps(holidayHours, tradingHours))
        subtractMInterval(tradingHours, holidayHours)
      else
        tradingHours
    }

  // returns true if the trading-schedule has any trading hours scheduled for that date; false otherwise.
  def isTradingDay(time: LocalDate, tradingSchedule: TradingSchedule): Boolean = !isEmpty(tradingSchedule(time))

  def tradingDays(startTime: LocalDate, tradingSchedule: TradingSchedule): Seq[LocalDate] =
    interspersedTimeSeries(startTime, Days.days(1)).filter(isTradingDay(_, tradingSchedule))

  def tradingDays(startDate: LocalDate, endDate: LocalDate, tradingSchedule: TradingSchedule): Seq[LocalDate] =
    tradingDays(startDate, tradingSchedule).takeWhile(isBeforeOrEqual(_, endDate))

  def nextTradingDay(date: LocalDate, tradingSchedule: TradingSchedule): LocalDate = {
    val nextDay = date.plus(Days.days(1))
    tradingDays(nextDay, tradingSchedule).head
  }
}