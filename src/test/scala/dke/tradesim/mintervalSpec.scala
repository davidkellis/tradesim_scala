package dke.tradesim

import org.scalatest.FunSpec
import datetimeUtils._
import minterval._

class mintervalSpec extends FunSpec {
  describe("createMInterval") {
    it("creates a vector of Intervals, sorted by Interval start time") {
      val inOrderIntervals = Vector(
        intervalBetween(datetime(2013, 1, 1, 8, 0, 0), datetime(2013, 1, 1, 12, 0, 0)),
        intervalBetween(datetime(2013, 1, 1, 12, 0, 0), datetime(2013, 1, 1, 17, 0, 0))
      )
      val outOfOrderIntervals = Vector(
        intervalBetween(datetime(2013, 1, 1, 12, 0, 0), datetime(2013, 1, 1, 17, 0, 0)),
          intervalBetween(datetime(2013, 1, 1, 8, 0, 0), datetime(2013, 1, 1, 12, 0, 0))
      )

      assert(createMInterval(outOfOrderIntervals) === inOrderIntervals)
    }
  }

  describe("subtractMInterval") {
    it("subtracts one MInterval from another") {
      val minuend = Vector(
        intervalBetween(datetime(2013, 1, 1, 8, 0, 0), datetime(2013, 1, 1, 12, 0, 0)),
        intervalBetween(datetime(2013, 1, 1, 12, 0, 0), datetime(2013, 1, 1, 17, 0, 0))
      )
      val subtrahend = Vector(
        intervalBetween(datetime(2013, 1, 1, 10, 0, 0), datetime(2013, 1, 1, 16, 0, 0))
      )
      val difference = Vector(
        intervalBetween(datetime(2013, 1, 1, 8, 0, 0), datetime(2013, 1, 1, 10, 0, 0)),
        intervalBetween(datetime(2013, 1, 1, 16, 0, 0), datetime(2013, 1, 1, 17, 0, 0))
      )

      assert(subtractMInterval(minuend, subtrahend) === difference)
    }
  }
}
