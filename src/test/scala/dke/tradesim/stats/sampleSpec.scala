package dke.tradesim.stats

import org.scalatest.FunSpec
import java.math.MathContext

import sample._

class sampleSpec extends FunSpec {
  describe("correlation") {
    // checked against wolfram alpha with this:
    //   N[Correlation[{1.59, 2.89, 3.76, 4.93, 5.0, 6.36, 7.35, 8.77, 9.19, 10.22}, {1.14, 2.54, 3.89, 4.18, 5.25, 6.3, 7.98, 8.54, 9.82, 10.41}], 20]
    it("returns the sample correlation coefficient (Pearson's r coefficient)") {
      val xs = List[BigDecimal](1.59, 2.89, 3.76, 4.93, 5.0, 6.36, 7.35, 8.77, 9.19, 10.22)
      val ys = List[BigDecimal](1.14, 2.54, 3.89, 4.18, 5.25, 6.3, 7.98, 8.54, 9.82, 10.41)
      assert(correlation(xs, ys).round(new MathContext(20)).toString === "0.99256620596044155460")
    }

  }

  describe("ols") {
    // checked against wolfram alpha with this:
    //   linear fit {1.59,1.14}, {2.89,2.54}, {3.76,3.89}, {4.93,4.18}
    it("performs a linear regression") {
      val xs = List[BigDecimal](1.59, 2.89, 3.76, 4.93)
      val ys = List[BigDecimal](1.14, 2.54, 3.89, 4.18)
      val result = ols(xs, ys)
      assert(result.slope.round(new MathContext(6)) === BigDecimal("0.956321"))
      assert(result.intercept.round(new MathContext(6)) === BigDecimal("-0.211186"))
    }
  }
}
