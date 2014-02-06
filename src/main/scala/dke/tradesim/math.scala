package dke.tradesim

import scala.math.BigDecimal.RoundingMode

object math {
  def ceil(decimal: BigDecimal): BigDecimal = decimal.setScale(0, RoundingMode.CEILING)

  def floor(decimal: BigDecimal): BigDecimal = decimal.setScale(0, RoundingMode.FLOOR)

  // see section on HALF_UP on http://docs.oracle.com/javase/6/docs/api/index.html?java/math/RoundingMode.html
  // -> on HALF_UP: "this is the rounding mode commonly taught at school"
  def round(decimal: BigDecimal): BigDecimal = decimal.setScale(0, RoundingMode.HALF_UP)
}