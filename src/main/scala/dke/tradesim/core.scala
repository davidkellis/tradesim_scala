package dke.tradesim

import org.joda.time.{DateTime}

object core {
  type StockHoldings = Map[String, Double]
  case class Portfolio(cash: Double,
                       stocks: StockHoldings)

  sealed abstract class Order {
    val symbol: String
    val qty: Double
    val fillPrice: Double
    def changeQty(newQty: Double): Order
  }
  sealed abstract class LimitOrder extends Order {
    def changeLimitPrice(newLimitPrice: Double): LimitOrder
  }
  sealed trait BuyOrder
  sealed trait SellOrder
  case class MarketBuy(time: DateTime, symbol: String, qty: Double, fillPrice: Double) extends Order with BuyOrder {
    def changeQty(newQty: Double): MarketBuy = this.copy(qty = newQty)
  }
  case class MarketSell(time: DateTime, symbol: String, qty: Double, fillPrice: Double) extends Order with SellOrder {
    def changeQty(newQty: Double): MarketSell = this.copy(qty = newQty)
  }
  case class LimitBuy(time: DateTime, symbol: String, qty: Double, limitPrice: Double, fillPrice: Double) extends LimitOrder with BuyOrder {
    def changeQty(newQty: Double): LimitBuy = this.copy(qty = newQty)
    def changeLimitPrice(newLimitPrice: Double): LimitBuy = this.copy(limitPrice = newLimitPrice)
  }
  case class LimitSell(time: DateTime, symbol: String, qty: Double, limitPrice: Double, fillPrice: Double) extends LimitOrder with SellOrder {
    def changeQty(newQty: Double): LimitSell = this.copy(qty = newQty)
    def changeLimitPrice(newLimitPrice: Double): LimitSell = this.copy(limitPrice = newLimitPrice)
  }

  case class State(previousTime: DateTime,
                   time: DateTime,
                   portfolio: Portfolio,
                   orders: Seq[Order],
                   transactions: Seq[Order])

  case class Trial(symbols: Seq[String],
                   principal: Double,
                   commissionPerTrade: Double,
                   commissionPerShare: Double,
                   startTime: DateTime,
                   endTime: DateTime,
                   incrementTime: (DateTime) => DateTime,
                   purchaseFillPrice: (DateTime, String) => Double,
                   saleFillPrice: (DateTime, String) => Double)

  case class Strategy(buildInitState: (Strategy, Trial) => State,
                      buildNextState: (Strategy, Trial, State) => State,
                      isFinalState: (Strategy, Trial, State) => Boolean)

  def defaultInitialState(time: DateTime, principal: Double) = State(time,
                                                                     time,
                                                                     Portfolio(principal, Map[String, Double]()),
                                                                     Vector(),
                                                                     Vector())
}
