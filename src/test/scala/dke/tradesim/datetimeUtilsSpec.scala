package dke.tradesim

//import org.junit.runner.RunWith
//import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec
import datetimeUtils._

//@RunWith(classOf[JUnitRunner])
class datetimeUtilsSpec extends FunSpec {
  describe("timestamp") {
    it("returns a timestamp representation, yyyymmddhhmmss, of a given DateTime") {
      assert(timestamp(datetime(2013, 7, 15, 20, 1, 45)) === 20130715200145L)
    }
  }

  describe("interspersedTimeSeries") {
    it("returns a sequence of DateTimes, [t1, t2, ..., tN], that are separated by a given Period, s.t. startTime = t1 and tN <= endTime") {
      val startTime = datetime(2013, 7, 1, 12, 0, 0)
      val endTime = datetime(2013, 7, 2, 12, 0, 0)

      val timeSeries = interspersedTimeSeries(startTime, endTime, hours(6)).toList
      val expectedTimeSeries = List(
        datetime(2013, 7, 1, 12, 0, 0),
        datetime(2013, 7, 1, 18, 0, 0),
        datetime(2013, 7, 2, 0, 0, 0),
        datetime(2013, 7, 2, 6, 0, 0),
        datetime(2013, 7, 2, 12, 0, 0)
      )
      assert(timeSeries === expectedTimeSeries)

      val timeSeries2 = interspersedTimeSeries(startTime, endTime, hours(7)).toList
      val expectedTimeSeries2 = List(
        datetime(2013, 7, 1, 12, 0, 0),
        datetime(2013, 7, 1, 19, 0, 0),
        datetime(2013, 7, 2, 2, 0, 0),
        datetime(2013, 7, 2, 9, 0, 0)
      )
      assert(timeSeries2 === expectedTimeSeries2)
    }
  }

  describe("interspersedIntervals") {
    it("returns a sequence of Intervals, [i1, i2, ..., iN], s.t. the start time of subsequent intervals is separated by a given Period, <separationLength> and each interval spans an amount of time given by <intervalLength>") {
      val startTime = datetime(2013, 7, 1, 12, 0, 0)
      val endTime = datetime(2013, 7, 2, 12, 0, 0)

      val intervals = interspersedIntervals(startTime, hours(5), days(1)).take(5).toList
      val expectedIntervals = List(
        intervalBetween(datetime(2013, 7, 1, 12, 0, 0), datetime(2013, 7, 1, 17, 0, 0)),
        intervalBetween(datetime(2013, 7, 2, 12, 0, 0), datetime(2013, 7, 2, 17, 0, 0)),
        intervalBetween(datetime(2013, 7, 3, 12, 0, 0), datetime(2013, 7, 3, 17, 0, 0)),
        intervalBetween(datetime(2013, 7, 4, 12, 0, 0), datetime(2013, 7, 4, 17, 0, 0)),
        intervalBetween(datetime(2013, 7, 5, 12, 0, 0), datetime(2013, 7, 5, 17, 0, 0))
      )
      assert(intervals === expectedIntervals)
    }
  }

  describe("isAnyHoliday") {
    it("returns true if the given date is a holiday") {
      assert(isAnyHoliday(date(2013, 3, 29)) === true)    // Good Friday
    }
  }

  describe("easter") {
    it("returns the DateTime that represents the beginning of the day on Easter, given a year") {
      assert(easter(2009) === date(2009, 4, 12))
      assert(easter(2010) === date(2010, 4, 4))
      assert(easter(2011) === date(2011, 4, 24))
      assert(easter(2012) === date(2012, 4, 8))
      assert(easter(2013) === date(2013, 3, 31))
    }
  }

  describe("goodFriday") {
    it("returns the DateTime that represents the beginning of the day of Good Friday, given a year") {
      assert(goodFriday(2009) === date(2009, 4, 10))
      assert(goodFriday(2010) === date(2010, 4, 2))
      assert(goodFriday(2011) === date(2011, 4, 22))
      assert(goodFriday(2012) === date(2012, 4, 6))
      assert(goodFriday(2013) === date(2013, 3, 29))
    }
  }
}
