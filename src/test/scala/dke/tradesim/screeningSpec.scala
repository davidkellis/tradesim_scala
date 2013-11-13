package dke.tradesim

import org.scalatest.FunSpec
import dke.tradesim.core.Security
import screening._

class screeningSpec extends FunSpec {
  describe("filter") {
    it("returns a subset of securites that pass the given predicate") {
      val aapl = Security(Some(123), "BBG1", "BBG1", "Stock", 1, "AAPL", "Apple", None, None, Some(555), Some(true), None, Some(4), Some(5))
      val aapy = Security(Some(124), "BBG2", "BBG2", "Stock", 1, "AAPY", "Aspyre", None, None, Some(556), Some(false), None, Some(4), Some(5))
      val securities = Seq(
        aapl,
        aapy
      )

      assert(filter(securities)(_.active) === Seq(aapl))
    }
  }

  describe("rank") {
    it("ranks securities by one or more orderings") {
      val aapl = Security(Some(3), "BBG1", "BBG2", "Stock", 1, "AAPL", "Apple", None, None, Some(1), Some(true), None, Some(4), Some(5))
      val aapy = Security(Some(1), "BBG2", "BBG3", "Stock", 1, "AAPY", "Aspyre", None, None, Some(2), Some(false), None, Some(4), Some(5))
      val aapz = Security(Some(2), "BBG3", "BBB1", "Stock", 1, "AAPZ", "AppZapper", None, None, Some(3), Some(false), None, Some(4), Some(5))
      val securities = Seq(
        aapl,
        aapz,
        aapy
      )

      val o1 = Ordering.by {s: Security => s.id }
      val o2 = Ordering.by {s: Security => s.bbGid }
      val o3 = Ordering.by {s: Security => s.bbGcid }

      assert(rank(securities, Seq(o1)) === Seq(aapy, aapz, aapl))
      assert(rankMap(securities, Seq(o1)) === Map((aapl -> 2), (aapy -> 0), (aapz -> 1)))

      assert(rank(securities, Seq(o2)) === Seq(aapl, aapy, aapz))
      assert(rankMap(securities, Seq(o2)) === Map((aapl -> 0), (aapy -> 1), (aapz -> 2)))

      assert(rank(securities, Seq(o3)) === Seq(aapz, aapl, aapy))
      assert(rankMap(securities, Seq(o3)) === Map((aapl -> 1), (aapy -> 2), (aapz -> 0)))

      assert(rank(securities, Seq(o2, o3)) === Seq(aapl, aapz, aapy))
      assert(rankMap(securities, Seq(o2, o3)) === Map((aapl -> 1), (aapy -> 3), (aapz -> 2)))
    }
  }

  describe("weighted rank") {
    it("ranks securities by one or more weighted orderings") {
      val aapl = Security(Some(3), "BBG1", "BBG2", "Stock", 1, "AAPL", "Apple", None, None, Some(1), Some(true), None, Some(4), Some(5))
      val aapy = Security(Some(1), "BBG2", "BBG3", "Stock", 1, "AAPY", "Aspyre", None, None, Some(2), Some(false), None, Some(4), Some(5))
      val aapz = Security(Some(2), "BBG3", "BBB1", "Stock", 1, "AAPZ", "AppZapper", None, None, Some(3), Some(false), None, Some(4), Some(5))
      val securities = Seq(
        aapl,
        aapz,
        aapy
      )

      val o1 = Ordering.by {s: Security => s.id }
      val o2 = Ordering.by {s: Security => s.bbGid }
      val o3 = Ordering.by {s: Security => s.bbGcid }

      assert(rank(securities, Map(o1 -> 1)) === Seq(aapy, aapz, aapl))
      assert(rankMap(securities, Map(o1 -> 1)) === Map((aapl -> 3), (aapy -> 1), (aapz -> 2)))

      assert(rank(securities, Map(o2 -> 2)) === Seq(aapl, aapy, aapz))
      assert(rankMap(securities, Map(o2 -> 2)) === Map((aapl -> 2), (aapy -> 4), (aapz -> 6)))

      assert(rank(securities, Map(o3 -> 3)) === Seq(aapz, aapl, aapy))
      assert(rankMap(securities, Map(o3 -> 3)) === Map((aapl -> 6), (aapy -> 9), (aapz -> 3)))

      assert(rank(securities, Map(o2 -> 1, o3 -> 10)) === Vector(aapz, aapl, aapy))
      assert(rankMap(securities, Map(o2 -> 1, o3 -> 10)) === Map((aapl -> 21), (aapy -> 32), (aapz -> 13)))
    }
  }
}