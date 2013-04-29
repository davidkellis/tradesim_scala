package dke.tradesim

import dke.tradesim.core._
import dke.tradesim.core.LimitBuy
import dke.tradesim.core.LimitSell
import org.joda.time.DateTime

object ordering {
  def isLimitOrder(order: Order): Boolean = order.isInstanceOf[LimitBuy] || order.isInstanceOf[LimitSell]

  def setOrderQty(order: Order, newQty: Long): Order = order.changeQty(newQty)

  def setLimitPrice(order: LimitOrder, newLimitPrice: BigDecimal): LimitOrder = order.changeLimitPrice(newLimitPrice)

  def sharesOnHand(portfolio: Portfolio, symbol: String): Long = portfolio.stocks.get(symbol).getOrElse(0)

  def addCash(portfolio: Portfolio, amount: BigDecimal): Portfolio = portfolio.copy(cash = portfolio.cash + amount)

  def setSharesOnHand(portfolio: Portfolio, symbol: String, qty: Long): Portfolio =
    portfolio.copy(stocks = portfolio.stocks + (symbol -> qty))

  def purchaseCost(qty: Long, price: BigDecimal, commissionPerTrade: BigDecimal, commissionPerShare: BigDecimal): BigDecimal =
    (qty * (price + commissionPerShare)) + commissionPerTrade
  def purchaseCost(time: DateTime,
                   symbol: String,
                   qty: Long,
                   commissionPerTrade: BigDecimal,
                   commissionPerShare: BigDecimal,
                   priceFn: PriceQuoteFn): BigDecimal =
    purchaseCost(qty, priceFn(time, symbol), commissionPerTrade, commissionPerShare)

  def saleProceeds(qty: Long, price: BigDecimal, commissionPerTrade: BigDecimal, commissionPerShare: BigDecimal): BigDecimal =
    (qty * (price - commissionPerShare)) + commissionPerTrade
  def saleProceeds(time: DateTime,
                   symbol: String,
                   qty: Long,
                   commissionPerTrade: BigDecimal,
                   commissionPerShare: BigDecimal,
                   priceFn: PriceQuoteFn): BigDecimal =
    saleProceeds(qty, priceFn(time, symbol), commissionPerTrade, commissionPerShare)

  def adjustPortfolioFromFilledOrder(trial: Trial, portfolio: Portfolio, order: Order): Portfolio = {
    val commissionPerTrade = trial.commissionPerTrade
    val commissionPerShare = trial.commissionPerShare
    val symbol = order.symbol
    val orderQty = order.qty
    val fillPrice = order.fillPrice.get
    val cashOnHand = portfolio.cash
    val sharesHeld = sharesOnHand(portfolio, symbol)
    order match {
      case _: BuyOrder =>   // adjust the portfolio for a purchase
        portfolio.copy(stocks = portfolio.stocks + (symbol -> (sharesHeld + orderQty)),
                       cash = cashOnHand - purchaseCost(orderQty, fillPrice, commissionPerTrade, commissionPerShare))
      case _: SellOrder =>  // adjust the portfolio for a sale
        portfolio.copy(stocks = portfolio.stocks + (symbol -> (sharesHeld - orderQty)),
                       cash = cashOnHand + saleProceeds(orderQty, fillPrice, commissionPerTrade, commissionPerShare))
    }
  }

  def maxSharesPurchasable(trial: Trial,
                           portfolio: Portfolio,
                           time: DateTime,
                           symbol: String,
                           bestOfferPriceFn: PriceQuoteFn): BigDecimal = {
    val principal = portfolio.cash
    val commissionPerTrade = trial.commissionPerTrade
    val commissionPerShare = trial.commissionPerShare
    (principal - commissionPerTrade).quot(bestOfferPriceFn(time, symbol) + commissionPerShare)
  }

  def isOrderFillable(order: MarketBuy, trial: Trial, portfolio: Portfolio, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Boolean =
    purchaseCost(order.time, order.symbol, order.qty, trial.commissionPerTrade, trial.commissionPerShare, purchaseFillPriceFn) <= portfolio.cash

  def isOrderFillable(order: MarketSell, trial: Trial, portfolio: Portfolio, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Boolean =
    saleProceeds(order.time, order.symbol, order.qty, trial.commissionPerTrade, trial.commissionPerShare, saleFillPriceFn) >= 0.0

  def isOrderFillable(order: LimitBuy, trial: Trial, portfolio: Portfolio, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Boolean =
    isOrderFillable(order.asInstanceOf[MarketBuy], trial, portfolio, purchaseFillPriceFn, saleFillPriceFn) && purchaseFillPriceFn(order.time, order.symbol) <= order.limitPrice

  def isOrderFillable(order: LimitSell, trial: Trial, portfolio: Portfolio, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Boolean =
    isOrderFillable(order.asInstanceOf[MarketSell], trial, portfolio, purchaseFillPriceFn, saleFillPriceFn) && saleFillPriceFn(order.time, order.symbol) >= order.limitPrice

  def isOrderFillable(order: Order, trial: Trial, portfolio: Portfolio, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Boolean =
    order match {
      case marketBuy: MarketBuy => isOrderFillable(marketBuy, trial, portfolio, purchaseFillPriceFn, saleFillPriceFn)
      case marketSell: MarketSell => isOrderFillable(marketSell, trial, portfolio, purchaseFillPriceFn, saleFillPriceFn)
      case limitBuy: LimitBuy => isOrderFillable(limitBuy, trial, portfolio, purchaseFillPriceFn, saleFillPriceFn)
      case limitSell: LimitSell => isOrderFillable(limitSell, trial, portfolio, purchaseFillPriceFn, saleFillPriceFn)
    }

  def orderFillPrice(order: BuyOrder, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): BigDecimal = purchaseFillPriceFn(order.time, order.symbol)
  def orderFillPrice(order: SellOrder, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): BigDecimal = saleFillPriceFn(order.time, order.symbol)
  def orderFillPrice(order: Order, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): BigDecimal = order match {
    case buyOrder: BuyOrder => orderFillPrice(buyOrder, purchaseFillPriceFn, saleFillPriceFn)
    case sellOrder: SellOrder => orderFillPrice(sellOrder, purchaseFillPriceFn, saleFillPriceFn)
  }

  def cancelAllPendingOrders(currentState: State): State = currentState.copy(orders = Vector())

  def buy(currentState: State, time: DateTime, symbol: String, qty: Long): State = {
    val newOrders = currentState.orders :+ MarketBuy(time, symbol, qty, None)
    currentState.copy(orders = newOrders)
  }

  def buyImmediately(currentState: State, symbol: String, qty: Long): State = buy(currentState, currentState.time, symbol, qty)

  def limitBuy(currentState: State, time: DateTime, symbol: String, qty: Long, limitPrice: BigDecimal): State = {
    val newOrders = currentState.orders :+ LimitBuy(time, symbol, qty, limitPrice, None)
    currentState.copy(orders = newOrders)
  }

  def sell(currentState: State, time: DateTime, symbol: String, qty: Long): State = {
    val newOrders = currentState.orders :+ MarketSell(time, symbol, qty, None)
    currentState.copy(orders = newOrders)
  }

  def sellImmediately(currentState: State, symbol: String, qty: Long): State = sell(currentState, currentState.time, symbol, qty)

  def limitSell(currentState: State, time: DateTime, symbol: String, qty: Long, limitPrice: BigDecimal): State = {
    val newOrders = currentState.orders :+ LimitSell(time, symbol, qty, limitPrice, None)
    currentState.copy(orders = newOrders)
  }

  def closeOpenStockPosition(currentState: State, symbol: String): State = {
    val qtyOnHand = sharesOnHand(currentState.portfolio, symbol)
    qtyOnHand match {
      case qty if qty > 0 => sellImmediately(currentState, symbol, qtyOnHand)   // we own shares, so sell them
      case qty if qty < 0 => buyImmediately(currentState, symbol, -qtyOnHand)    // we owe a share debt, so buy those shares back (we negate qtyOnHand because it is negative, and we want to buy a positive quantity)
      case 0 => currentState
    }
  }

  def closeAllOpenStockPositions(currentState: State): State = {
    val stocks = currentState.portfolio.stocks
    if (!stocks.isEmpty)
      stocks.keys.foldLeft(currentState)(closeOpenStockPosition)
    else
      currentState
  }
}
