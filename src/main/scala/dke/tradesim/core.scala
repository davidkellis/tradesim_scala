package dke.tradesim

import org.joda.time.{DateTime}

object core {
  type StockHoldings = Map[String, Long]

  case class Portfolio(cash: BigDecimal,
                       stocks: StockHoldings)

  sealed trait Order {
    val time: DateTime
    val symbol: String
    val qty: Long
    val fillPrice: Option[BigDecimal]
    def changeQty(newQty: Long): Order
    def changeFillPrice(newFillPrice: BigDecimal): Order
  }

  type MarketOrder = Order
  sealed trait LimitOrder extends Order {
    val limitPrice: BigDecimal
    def changeLimitPrice(newLimitPrice: BigDecimal): LimitOrder
  }

  sealed trait BuyOrder extends Order
  sealed trait SellOrder extends Order

  case class MarketBuy(time: DateTime, symbol: String, qty: Long, fillPrice: Option[BigDecimal] = None) extends MarketOrder with BuyOrder {
    def changeQty(newQty: Long): MarketBuy = this.copy(qty = newQty)
    def changeFillPrice(newFillPrice: BigDecimal): MarketBuy = this.copy(fillPrice = Option(newFillPrice))
  }

  case class MarketSell(time: DateTime, symbol: String, qty: Long, fillPrice: Option[BigDecimal] = None) extends MarketOrder with SellOrder {
    def changeQty(newQty: Long): MarketSell = this.copy(qty = newQty)
    def changeFillPrice(newFillPrice: BigDecimal): MarketSell = this.copy(fillPrice = Option(newFillPrice))
  }

  case class LimitBuy(time: DateTime, symbol: String, qty: Long, limitPrice: BigDecimal, fillPrice: Option[BigDecimal] = None) extends LimitOrder with BuyOrder {
    def changeQty(newQty: Long): LimitBuy = this.copy(qty = newQty)
    def changeLimitPrice(newLimitPrice: BigDecimal): LimitBuy = this.copy(limitPrice = newLimitPrice)
    def changeFillPrice(newFillPrice: BigDecimal): LimitBuy = this.copy(fillPrice = Option(newFillPrice))
  }

  case class LimitSell(time: DateTime, symbol: String, qty: Long, limitPrice: BigDecimal, fillPrice: Option[BigDecimal] = None) extends LimitOrder with SellOrder {
    def changeQty(newQty: Long): LimitSell = this.copy(qty = newQty)
    def changeLimitPrice(newLimitPrice: BigDecimal): LimitSell = this.copy(limitPrice = newLimitPrice)
    def changeFillPrice(newFillPrice: BigDecimal): LimitSell = this.copy(fillPrice = Option(newFillPrice))
  }

  type PriceQuoteFn = (DateTime, String) => BigDecimal

  case class State(previousTime: DateTime,
                   time: DateTime,
                   portfolio: Portfolio,
                   orders: Seq[Order],
                   transactions: Seq[Order])

  case class Trial(symbols: Seq[String],
                   principal: BigDecimal,
                   commissionPerTrade: BigDecimal,
                   commissionPerShare: BigDecimal,
                   startTime: DateTime,
                   endTime: DateTime,
                   incrementTime: (DateTime) => DateTime,
                   purchaseFillPrice: PriceQuoteFn,
                   saleFillPrice: PriceQuoteFn)

  case class Strategy(buildInitState: (Strategy, Trial) => State,
                      buildNextState: (Strategy, Trial, State) => State,
                      isFinalState: (Strategy, Trial, State) => Boolean)

  abstract class Bar {
    val symbol: String
    val startTime: DateTime
    val endTime: DateTime
    val open: BigDecimal
    val high: BigDecimal
    val low: BigDecimal
    val close: BigDecimal
    val volume: Long
  }

  case class EodBar(symbol: String,
                    startTime: DateTime,
                    endTime: DateTime,
                    open: BigDecimal,
                    high: BigDecimal,
                    low: BigDecimal,
                    close: BigDecimal,
                    volume: Long) extends Bar

  def defaultInitialState(time: DateTime, principal: BigDecimal) = State(time,
                                                                         time,
                                                                         Portfolio(principal, Map[String, Long]()),
                                                                         Vector(),
                                                                         Vector())

  // like the -> (thread) operator in clojure
  def threadThrough[T](o: T, fns: (T => T)*): T = fns.foldLeft(o)((intermediateObject, transform) => transform(intermediateObject))
}
