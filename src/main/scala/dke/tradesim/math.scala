package dke.tradesim

import scala.math.BigDecimal.RoundingMode

object math {
  def floor(decimal: BigDecimal): BigDecimal = decimal.setScale(0, RoundingMode.FLOOR)
}