package dke.tradesim

import dke.tradesim.core._
import dke.tradesim.core.LimitBuy
import dke.tradesim.core.LimitSell
import org.joda.time.DateTime

object ordering {
  def isLimitOrder(order: Order): Boolean = order.isInstanceOf[LimitBuy] || order.isInstanceOf[LimitSell]

  def setOrderQty(order: Order, newQty: Long): Order = order.changeQty(newQty)
  def setOrderQty(order: LimitOrder, newQty: Long): LimitOrder = order.changeQty(newQty).asInstanceOf[LimitOrder]

  def setLimitPrice(order: LimitOrder, newLimitPrice: BigDecimal): LimitOrder = order.changeLimitPrice(newLimitPrice)

  def sharesOnHand(portfolio: Portfolio, securityId: SecurityId): Long = portfolio.stocks.get(securityId).getOrElse(0)

  def addCash(portfolio: Portfolio, amount: BigDecimal): Portfolio = portfolio.copy(cash = portfolio.cash + amount)

  def setSharesOnHand(portfolio: Portfolio, securityId: SecurityId, qty: Long): Portfolio =
    portfolio.copy(stocks = portfolio.stocks + (securityId -> qty))

  def purchaseCost(qty: Long, price: BigDecimal, commissionPerTrade: BigDecimal, commissionPerShare: BigDecimal): BigDecimal =
    (qty * (price + commissionPerShare)) + commissionPerTrade

  def purchaseCost(time: DateTime,
                   securityId: SecurityId,
                   qty: Long,
                   commissionPerTrade: BigDecimal,
                   commissionPerShare: BigDecimal,
                   priceFn: PriceQuoteFn): Option[BigDecimal] = {
    val price = priceFn(time, securityId)
    price.map(price => purchaseCost(qty, price, commissionPerTrade, commissionPerShare))
  }

  def saleProceeds(qty: Long, price: BigDecimal, commissionPerTrade: BigDecimal, commissionPerShare: BigDecimal): BigDecimal =
    (qty * (price - commissionPerShare)) + commissionPerTrade

  def saleProceeds(time: DateTime,
                   securityId: SecurityId,
                   qty: Long,
                   commissionPerTrade: BigDecimal,
                   commissionPerShare: BigDecimal,
                   priceFn: PriceQuoteFn): Option[BigDecimal] = {
    val price = priceFn(time, securityId)
    price.map(price => saleProceeds(qty, price, commissionPerTrade, commissionPerShare))
  }

  def adjustPortfolioFromFilledOrder(trial: Trial, portfolio: Portfolio, order: Order): Portfolio = {
    val commissionPerTrade = trial.commissionPerTrade
    val commissionPerShare = trial.commissionPerShare
    val securityId = order.securityId
    val orderQty = order.qty
    val fillPrice = order.fillPrice.get
    val cashOnHand = portfolio.cash
    val sharesHeld = sharesOnHand(portfolio, securityId)
    order match {
      case _: BuyOrder =>   // adjust the portfolio for a purchase
        portfolio.copy(stocks = portfolio.stocks + (securityId -> (sharesHeld + orderQty)),
                       cash = cashOnHand - purchaseCost(orderQty, fillPrice, commissionPerTrade, commissionPerShare))
      case _: SellOrder =>  // adjust the portfolio for a sale
        portfolio.copy(stocks = portfolio.stocks + (securityId -> (sharesHeld - orderQty)),
                       cash = cashOnHand + saleProceeds(orderQty, fillPrice, commissionPerTrade, commissionPerShare))
    }
  }

  def maxSharesPurchasable(trial: Trial,
                           portfolio: Portfolio,
                           time: DateTime,
                           securityId: SecurityId,
                           bestOfferPriceFn: PriceQuoteFn): Option[BigDecimal] = {
    val principal = portfolio.cash
    val commissionPerTrade = trial.commissionPerTrade
    val commissionPerShare = trial.commissionPerShare
    val price = bestOfferPriceFn(time, securityId)
    price.map(price => (principal - commissionPerTrade).quot(price + commissionPerShare))
  }

  def isOrderFillable(order: MarketBuy, time: DateTime, trial: Trial, portfolio: Portfolio, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Boolean = {
    val cost = purchaseCost(time, order.securityId, order.qty, trial.commissionPerTrade, trial.commissionPerShare, purchaseFillPriceFn)
//    println(s"isOrderFillable(MarketBuy): ${cost.get} <= ${portfolio.cash} = ${cost.get <= portfolio.cash}")
    cost.map(_ <= portfolio.cash).getOrElse(false)
  }

  def isOrderFillable(order: MarketSell, time: DateTime, trial: Trial, portfolio: Portfolio, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Boolean = {
    val proceeds = saleProceeds(time, order.securityId, order.qty, trial.commissionPerTrade, trial.commissionPerShare, saleFillPriceFn)
    proceeds.map(_ >= 0.0).getOrElse(false)
  }

  def isOrderFillable(order: LimitBuy, time: DateTime, trial: Trial, portfolio: Portfolio, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Boolean = {
    val fillPrice = purchaseFillPriceFn(time, order.securityId)
    fillPrice.map(_ <= order.limitPrice).getOrElse(false) && isOrderFillable(order.asInstanceOf[MarketBuy], time, trial, portfolio, purchaseFillPriceFn, saleFillPriceFn)
  }

  def isOrderFillable(order: LimitSell, time: DateTime, trial: Trial, portfolio: Portfolio, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Boolean = {
    val fillPrice = saleFillPriceFn(time, order.securityId)
    fillPrice.map(_ >= order.limitPrice).getOrElse(false) && isOrderFillable(order.asInstanceOf[MarketSell], time, trial, portfolio, purchaseFillPriceFn, saleFillPriceFn)
  }

  def isOrderFillable(order: Order, time: DateTime, trial: Trial, portfolio: Portfolio, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Boolean =
    order match {
      case marketBuy: MarketBuy => isOrderFillable(marketBuy, time, trial, portfolio, purchaseFillPriceFn, saleFillPriceFn)
      case marketSell: MarketSell => isOrderFillable(marketSell, time, trial, portfolio, purchaseFillPriceFn, saleFillPriceFn)
      case limitBuy: LimitBuy => isOrderFillable(limitBuy, time, trial, portfolio, purchaseFillPriceFn, saleFillPriceFn)
      case limitSell: LimitSell => isOrderFillable(limitSell, time, trial, portfolio, purchaseFillPriceFn, saleFillPriceFn)
    }

  def orderFillPrice(order: BuyOrder, time: DateTime, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Option[BigDecimal] = purchaseFillPriceFn(time, order.securityId)
  def orderFillPrice(order: SellOrder, time: DateTime, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Option[BigDecimal] = saleFillPriceFn(time, order.securityId)
  def orderFillPrice(order: Order, time: DateTime, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Option[BigDecimal] = order match {
    case buyOrder: BuyOrder => orderFillPrice(buyOrder, time, purchaseFillPriceFn, saleFillPriceFn)
    case sellOrder: SellOrder => orderFillPrice(sellOrder, time, purchaseFillPriceFn, saleFillPriceFn)
  }

  def cancelAllPendingOrders[StateT <: State[StateT]](currentState: StateT): StateT = currentState.copy(orders = Vector())

  def buy[StateT <: State[StateT]](currentState: StateT, time: DateTime, securityId: SecurityId, qty: Long): StateT = {
    val newOrders = currentState.orders :+ MarketBuy(time, securityId, qty, None)
    currentState.copy(orders = newOrders)
  }

  def buyImmediately[StateT <: State[StateT]](currentState: StateT, securityId: SecurityId, qty: Long): StateT = buy(currentState, currentState.time, securityId, qty)

  def limitBuy[StateT <: State[StateT]](currentState: StateT, time: DateTime, securityId: SecurityId, qty: Long, limitPrice: BigDecimal): StateT = {
    val newOrders = currentState.orders :+ LimitBuy(time, securityId, qty, limitPrice, None)
    currentState.copy(orders = newOrders)
  }

  def sell[StateT <: State[StateT]](currentState: StateT, time: DateTime, securityId: SecurityId, qty: Long): StateT = {
    val newOrders = currentState.orders :+ MarketSell(time, securityId, qty, None)
    currentState.copy(orders = newOrders)
  }

  def sellImmediately[StateT <: State[StateT]](currentState: StateT, securityId: SecurityId, qty: Long): StateT = sell(currentState, currentState.time, securityId, qty)

  def limitSell[StateT <: State[StateT]](currentState: StateT, time: DateTime, securityId: SecurityId, qty: Long, limitPrice: BigDecimal): StateT = {
    val newOrders = currentState.orders :+ LimitSell(time, securityId, qty, limitPrice, None)
    currentState.copy(orders = newOrders)
  }

  def closeOpenStockPosition[StateT <: State[StateT]](currentState: StateT, securityId: SecurityId): StateT = {
    val qtyOnHand = sharesOnHand(currentState.portfolio, securityId)
    qtyOnHand match {
      case qty if qty > 0 => sellImmediately(currentState, securityId, qtyOnHand)    // we own shares, so sell them
      case qty if qty < 0 => buyImmediately(currentState, securityId, -qtyOnHand)    // we owe a share debt, so buy those shares back (we negate qtyOnHand because it is negative, and we want to buy a positive quantity)
      case 0 => currentState
    }
  }

  def closeAllOpenStockPositions[StateT <: State[StateT]](currentState: StateT): StateT = {
    val stocks = currentState.portfolio.stocks
    if (!stocks.isEmpty)
      stocks.keys.foldLeft(currentState)(closeOpenStockPosition)
    else
      currentState
  }
}
