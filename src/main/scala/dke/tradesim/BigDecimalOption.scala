package dke.tradesim

object BigDecimalOption {
  object implicits {
    implicit def convertOptionToBigDecimalOption(operand: Option[BigDecimal]): BigDecimalOption = BigDecimalOption(operand)
    implicit def convertBigDecimalToBigDecimalOption(operand: BigDecimal): BigDecimalOption = BigDecimalOption(Some(operand))
  }

  def apply(n: Option[BigDecimal]) = new BigDecimalOption(n)
  def apply(n: BigDecimal) = new BigDecimalOption(Some(n))

  def unapply(bdOption: BigDecimalOption): Option[Option[BigDecimal]] = if (bdOption == null) None else Some(bdOption.n)
}

class BigDecimalOption(val n: Option[BigDecimal]) extends Ordered[BigDecimalOption] {
  import BigDecimalOption.implicits.convertOptionToBigDecimalOption

  def map(f: BigDecimal => BigDecimal): Option[BigDecimal] = n.map(f)
  def flatMap(f: BigDecimal => Option[BigDecimal]): Option[BigDecimal] = if (n.isDefined) f(n.get) else None
  def filter(p: BigDecimal => Boolean): Option[BigDecimal] = n.filter(p)

//  def +(m: BigDecimalOption): BigDecimalOption = for {
//    r <- m
//    l <- n
//  } yield l + r
//  def +(m: BigDecimalOption): BigDecimalOption = m.flatMap(r => n.map(l => l + r))
  def +(m: BigDecimalOption): BigDecimalOption = n.flatMap(l => m.map(r => l + r))

  def -(m: BigDecimalOption): BigDecimalOption = for {
    l <- n
    r <- m
  } yield l - r

  def *(m: BigDecimalOption): BigDecimalOption = for {
    l <- n
    r <- m
  } yield l * r

  def /(m: BigDecimalOption): BigDecimalOption = for {
    l <- n
    r <- m
  } yield l / r

  override def equals(other: Any): Boolean =
    other match {
      case BigDecimalOption(m) => n == m
      case m @ Some(_: BigDecimal) => n == m
      case m: BigDecimal => n == Some(m)
      case _ => false
    }

//  def canEqual(other: Any): Boolean = other.isInstanceOf[BigDecimalOption]    // this is essentially just a null check

  override def hashCode: Int = n.map{ x: BigDecimal => (x * 41).hashCode() }.getOrElse(-1)

  def compare(that: BigDecimalOption): Int = {
    n match {
      case Some(l) =>
        that match {
          case BigDecimalOption(Some(r)) => l.compare(r)    // both l and r are defined
          case _ => 1                                       // l is defined, r is not => l > r
        }
      case None =>
        that match {
          case BigDecimalOption(Some(r)) => -1              // l is not defined, r is => l < r
          case _ => 0                                       // neither l nor r is defined => they are "equal"
        }
    }
  }
}


//import BigDecimalOption.implicits._
//BigDecimalOption(4) < BigDecimal(5)
//BigDecimalOption(4) == Some(BigDecimal(4))