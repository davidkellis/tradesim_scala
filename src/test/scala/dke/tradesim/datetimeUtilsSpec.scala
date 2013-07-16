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
