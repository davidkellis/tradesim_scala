package dke.tradesim

import dke.tradesim.core._
import dke.tradesim.core.LimitBuy
import dke.tradesim.core.LimitSell
import org.joda.time.DateTime

object ordering {
  def isLimitOrder(order: Order): Boolean = order.isInstanceOf[LimitBuy] || order.isInstanceOf[LimitSell]

  def setOrderQty(order: Order, newQty: Double): Order = order.changeQty(newQty)

  def setLimitPrice(order: LimitOrder, newLimitPrice: Double): LimitOrder = order.changeLimitPrice(newLimitPrice)

  def sharesOnHand(portfolio: Portfolio, symbol: String): Double = portfolio.stocks.get(symbol).getOrElse(0)

  def addCash(portfolio: Portfolio, amount: Double): Portfolio = portfolio.copy(cash = portfolio.cash + amount)

  def setSharesOnHand(portfolio: Portfolio, symbol: String, count: Double): Portfolio =
    portfolio.copy(stocks = portfolio.stocks + (symbol -> count))

  def purchaseCost(qty: Double, price: Double, commissionPerTrade: Double, commissionPerShare: Double): Double =
    (qty * (price + commissionPerShare)) + commissionPerTrade
  def purchaseCost(time: DateTime,
                   symbol: String,
                   qty: Double,
                   commissionPerTrade: Double,
                   commissionPerShare: Double,
                   priceFn: (DateTime, String) => Double): Double =
    purchaseCost(qty, priceFn(time, symbol), commissionPerTrade, commissionPerShare)

  def saleProceeds(qty: Double, price: Double, commissionPerTrade: Double, commissionPerShare: Double): Double =
    (qty * (price - commissionPerShare)) + commissionPerTrade
  def saleProceeds(time: DateTime,
                   symbol: String,
                   qty: Double,
                   commissionPerTrade: Double,
                   commissionPerShare: Double,
                   priceFn: (DateTime, String) => Double): Double =
    saleProceeds(qty, priceFn(time, symbol), commissionPerTrade, commissionPerShare)

  def adjustPortfolioFromFilledOrder(trial: Trial, portfolio: Portfolio, order: Order): Portfolio = {
    val commissionPerTrade = trial.commissionPerTrade
    val commissionPerShare = trial.commissionPerShare
    val symbol = order.symbol
    val orderQty = order.qty
    val fillPrice = order.fillPrice
    val cashOnHand = portfolio.cash
    val sharesHeld = sharesOnHand(portfolio, symbol)
    order match {
      case _: BuyOrder =>   // adjust the portfolio for a purchase
        portfolio.copy(stocks = portfolio.stocks + (symbol -> sharesHeld + orderQty),
                       cash = cashOnHand - purchaseCost(orderQty, fillPrice, commissionPerTrade, commissionPerShare))
      case _: SellOrder =>  // adjust the portfolio for a sale
        portfolio.copy(stocks = portfolio.stocks + (symbol -> sharesHeld - orderQty),
                       cash = cashOnHand + saleProceeds(orderQty, fillPrice, commissionPerTrade, commissionPerShare))
    }
  }
}
