package dke.tradesim

object BigDecimalOption {
  implicit def convertOptionToBigDecimalOption(operand: Option[BigDecimal]): BigDecimalOption = new BigDecimalOption(operand)
}

class BigDecimalOption(val n: Option[BigDecimal]) extends Ordered[BigDecimalOption] {
  def +(m: Option[BigDecimal]): Option[BigDecimal] = for {
    l <- n
    r <- m
  } yield l + r

  def -(m: Option[BigDecimal]): Option[BigDecimal] = for {
    l <- n
    r <- m
  } yield l - r

  def *(m: Option[BigDecimal]): Option[BigDecimal] = for {
    l <- n
    r <- m
  } yield l * r

  def /(m: Option[BigDecimal]): Option[BigDecimal] = for {
    l <- n
    r <- m
  } yield l / r

  override def equals(other: Any): Boolean =
    other match {
      case that: BigDecimalOption =>
        (that canEqual this) &&
        ( n == that.n ||
          n.getOrElse(null) == that.n.getOrElse(null) )
      case _ => false
    }

  def canEqual(other: Any): Boolean = other.isInstanceOf[BigDecimalOption]

  override def hashCode: Int = n.map{ x: BigDecimal => (x * 41).hashCode() }.getOrElse(-1)
  
  def compare(that: BigDecimalOption): Int = {
    n match {
      case Some(l) =>
        that.n match {
          case Some(r) => l.compare(r)
          case None => -1
        }
      case None =>
        that.n match {
          case Some(r) => 1
          case None => 0
        }
    }
  }
}
