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
  }
  type MarketOrder = Order
  sealed trait LimitOrder extends Order {
    val limitPrice: BigDecimal
    def changeLimitPrice(newLimitPrice: BigDecimal): LimitOrder
  }
  sealed trait BuyOrder extends Order
  sealed trait SellOrder extends Order
  case class MarketBuy(time: DateTime, symbol: String, qty: Long, fillPrice: Option[BigDecimal]) extends BuyOrder {
    def changeQty(newQty: Long): MarketBuy = this.copy(qty = newQty)
  }
  case class MarketSell(time: DateTime, symbol: String, qty: Long, fillPrice: Option[BigDecimal]) extends SellOrder {
    def changeQty(newQty: Long): MarketSell = this.copy(qty = newQty)
  }
  case class LimitBuy(time: DateTime, symbol: String, qty: Long, limitPrice: BigDecimal, fillPrice: Option[BigDecimal]) extends LimitOrder with BuyOrder {
    def changeQty(newQty: Long): LimitBuy = this.copy(qty = newQty)
    def changeLimitPrice(newLimitPrice: BigDecimal): LimitBuy = this.copy(limitPrice = newLimitPrice)
  }
  case class LimitSell(time: DateTime, symbol: String, qty: Long, limitPrice: BigDecimal, fillPrice: Option[BigDecimal]) extends LimitOrder with SellOrder {
    def changeQty(newQty: Long): LimitSell = this.copy(qty = newQty)
    def changeLimitPrice(newLimitPrice: BigDecimal): LimitSell = this.copy(limitPrice = newLimitPrice)
  }

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
                   purchaseFillPrice: (DateTime, String) => BigDecimal,
                   saleFillPrice: (DateTime, String) => BigDecimal)

  case class Strategy(buildInitState: (Strategy, Trial) => State,
                      buildNextState: (Strategy, Trial, State) => State,
                      isFinalState: (Strategy, Trial, State) => Boolean)

  def defaultInitialState(time: DateTime, principal: BigDecimal) = State(time,
                                                                         time,
                                                                         Portfolio(principal, Map[String, Long]()),
                                                                         Vector(),
                                                                         Vector())
}
