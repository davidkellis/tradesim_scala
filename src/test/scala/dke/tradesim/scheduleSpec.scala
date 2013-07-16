package dke.tradesim

import org.scalatest.FunSpec
import datetimeUtils._
import schedule._
import minterval._

class scheduleSpec extends FunSpec {
  describe("buildTradingSchedule") {
    it("returns a single-argument function that returns an MInterval representing the time interval on the given date that is considered to be 'in' the schedule") {
      val tradingSchedule = buildTradingSchedule(defaultTradingSchedule _, defaultHolidaySchedule _)

      val regularBusinessHoursInterval = intervalBetween(datetime(2013, 3, 28, 8, 30, 0), datetime(2013, 3, 28, 15, 0, 0))
      assert(tradingSchedule(date(2013, 3, 28)) === createMInterval(Vector(regularBusinessHoursInterval)))

      val holidayHours = emptyMInterval
      assert(tradingSchedule(date(2013, 3, 29)) === holidayHours)
    }
  }

  describe("isTradingDay") {
    it("returns true if the given LocalDate represents a trading day, according to the given trading schedule") {
      val tradingSchedule = buildTradingSchedule(defaultTradingSchedule _, defaultHolidaySchedule _)
      assert(isTradingDay(date(2013, 3, 29), tradingSchedule) === false)
    }
  }

  describe("tradingDays") {
    it("returns a sequence of LocalDates representing the trading schedule of a given time period") {
      val tradingSchedule = buildTradingSchedule(defaultTradingSchedule _, defaultHolidaySchedule _)
      val days = tradingDays(date(2013, 3, 27), tradingSchedule).take(4)
      assert(days.toList === date(2013, 3, 27) :: date(2013, 3, 28) :: date(2013, 4, 1) :: date(2013, 4, 2) :: Nil)
    }
  }
}
