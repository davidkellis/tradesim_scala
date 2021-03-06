package dke.tradesim

import org.joda.time.DateTime
import dke.tradesim.core._
import dke.tradesim.math._
import dke.tradesim.adjustedQuotes._

object ordering {
  def isLimitOrder(order: Order): Boolean = order.isInstanceOf[LimitBuy] || order.isInstanceOf[LimitSell]

  def setOrderQty(order: Order, newQty: Long): Order = order.changeQty(newQty)
  def setOrderQty(order: LimitOrder, newQty: Long): LimitOrder = order.changeQty(newQty).asInstanceOf[LimitOrder]

  def setLimitPrice(order: LimitOrder, newLimitPrice: BigDecimal): LimitOrder = order.changeLimitPrice(newLimitPrice)

  def sharesOnHand(portfolio: Portfolio, securityId: SecurityId): Long = portfolio.stocks.get(securityId).getOrElse(0)

  def addCash(portfolio: Portfolio, amount: BigDecimal): Portfolio = portfolio.copy(cash = portfolio.cash + amount)

  def setSharesOnHand(portfolio: Portfolio, securityId: SecurityId, qty: Long): Portfolio =
    portfolio.copy(stocks = portfolio.stocks + (securityId -> qty))

  def cashOnHand[StateT <: State[StateT]](state: StateT): BigDecimal = state.portfolio.cash
  def cashOnHand(portfolio: Portfolio): BigDecimal = portfolio.cash

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
    (qty * (price - commissionPerShare)) - commissionPerTrade

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
                           bestOfferPriceFn: PriceQuoteFn): Option[BigDecimal] =
    maxSharesPurchasable(trial, portfolio.cash, time, securityId, bestOfferPriceFn)

  def maxSharesPurchasable(trial: Trial,
                           principal: BigDecimal,
                           time: DateTime,
                           securityId: SecurityId,
                           bestOfferPriceFn: PriceQuoteFn): Option[BigDecimal] = {
    val commissionPerTrade = trial.commissionPerTrade
    val commissionPerShare = trial.commissionPerShare
    val price = bestOfferPriceFn(time, securityId)
    price.map(price => (principal - commissionPerTrade).quot(price + commissionPerShare))
  }

  def isOrderFillable(order: MarketBuy, time: DateTime, trial: Trial, portfolio: Portfolio, purchaseFillPriceFn: PriceQuoteFn): Boolean = {
    val cost = purchaseCost(time, order.securityId, order.qty, trial.commissionPerTrade, trial.commissionPerShare, purchaseFillPriceFn)

    // todo: I think this requirement should be removed because http://www.21stcenturyinvestoreducation.com/page/tce/courses/course-101/005/001-cash-vs-margin.html
    //       says even cash accounts can temporarily have a negative cash balance as long as the necessary funds are depositied within 3 business days after
    //       the purchase.
    cost.map(_ <= portfolio.cash).getOrElse(false)    // this condition is only applicable to cash-only accounts; this is allowed in margin accounts
    // the condition should be that the post-purchase cash balance should be within reasonable margin requirements.
  }

  def isOrderFillable(order: MarketSell, time: DateTime, trial: Trial, portfolio: Portfolio, saleFillPriceFn: PriceQuoteFn): Boolean = {
    val proceeds = saleProceeds(time, order.securityId, order.qty, trial.commissionPerTrade, trial.commissionPerShare, saleFillPriceFn)
    proceeds.map(_ >= 0.0).getOrElse(false)
  }

  def isOrderFillable(order: LimitBuy, time: DateTime, trial: Trial, portfolio: Portfolio, purchaseFillPriceFn: PriceQuoteFn): Boolean = {
    val fillPrice = purchaseFillPriceFn(time, order.securityId)
    fillPrice.map(_ <= order.limitPrice).getOrElse(false) && isOrderFillable(order.asInstanceOf[MarketBuy], time, trial, portfolio, purchaseFillPriceFn)
  }

  def isOrderFillable(order: LimitSell, time: DateTime, trial: Trial, portfolio: Portfolio, saleFillPriceFn: PriceQuoteFn): Boolean = {
    val fillPrice = saleFillPriceFn(time, order.securityId)
    fillPrice.map(_ >= order.limitPrice).getOrElse(false) && isOrderFillable(order.asInstanceOf[MarketSell], time, trial, portfolio, saleFillPriceFn)
  }

  def isOrderFillable(order: Order, time: DateTime, trial: Trial, portfolio: Portfolio, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Boolean =
    order match {
      case marketBuy: MarketBuy => isOrderFillable(marketBuy, time, trial, portfolio, purchaseFillPriceFn)
      case marketSell: MarketSell => isOrderFillable(marketSell, time, trial, portfolio, saleFillPriceFn)
      case limitBuy: LimitBuy => isOrderFillable(limitBuy, time, trial, portfolio, purchaseFillPriceFn)
      case limitSell: LimitSell => isOrderFillable(limitSell, time, trial, portfolio, saleFillPriceFn)
    }

  def orderFillPrice(order: BuyOrder, time: DateTime, purchaseFillPriceFn: PriceQuoteFn): Option[BigDecimal] = purchaseFillPriceFn(time, order.securityId)
  def orderFillPrice(order: SellOrder, time: DateTime, saleFillPriceFn: PriceQuoteFn): Option[BigDecimal] = saleFillPriceFn(time, order.securityId)
  def orderFillPrice(order: Order, time: DateTime, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Option[BigDecimal] = order match {
    case buyOrder: BuyOrder => orderFillPrice(buyOrder, time, purchaseFillPriceFn)
    case sellOrder: SellOrder => orderFillPrice(sellOrder, time, saleFillPriceFn)
  }

  def cancelAllPendingOrders[StateT <: State[StateT]](currentState: StateT): StateT = currentState.copy(orders = Vector())

  def buy[StateT <: State[StateT]](currentState: StateT, time: DateTime, securityId: SecurityId, qty: Long): StateT = {
    val newOrders = currentState.orders :+ MarketBuy(time, securityId, qty, None)
    currentState.copy(orders = newOrders)
  }

  def buyImmediately[StateT <: State[StateT]](currentState: StateT, securityId: SecurityId, qty: Long): StateT = buy(currentState, currentState.time, securityId, qty)

  def buyEqually[StateT <: State[StateT]](trial: Trial, currentState: StateT, securityIds: IndexedSeq[SecurityId], bestOfferPriceFn: PriceQuoteFn): StateT = {
    val count = securityIds.length
    val cash = cashOnHand(currentState)
    val principalPerSecurity = cash / count
    securityIds.foldLeft(currentState) { (state, securityId) =>
      val qty = maxSharesPurchasable(trial, principalPerSecurity, currentState.time, securityId, bestOfferPriceFn)
      qty.map(qty => buyImmediately(state, securityId, floor(qty).toLong)).getOrElse(state)
    }
  }

  def limitBuy[StateT <: State[StateT]](currentState: StateT, time: DateTime, securityId: SecurityId, qty: Long, limitPrice: BigDecimal): StateT = {
    val newOrders = currentState.orders :+ LimitBuy(time, securityId, qty, limitPrice, None)
    currentState.copy(orders = newOrders)
  }

  // this should be merged with sellImmediately
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
