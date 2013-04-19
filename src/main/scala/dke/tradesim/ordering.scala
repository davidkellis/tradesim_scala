package dke.tradesim

import dke.tradesim.core._
import dke.tradesim.core.LimitBuy
import dke.tradesim.core.LimitSell
import org.joda.time.DateTime

object ordering {
  type PriceQuoteFn = (DateTime, String) => BigDecimal

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

//  def isMarketBuyFillable(trial: Trial, portfolio: Portfolio, order: MarketOrder, fillPriceFn: PriceQuoteFn): Boolean =
//    purchaseCost(order.time, order.symbol, order.qty, trial.commissionPerTrade, trial.commissionPerShare, fillPriceFn) <= portfolio.cash
//
//  def isMarketSellFillable(trial: Trial, portfolio: Portfolio, order: MarketOrder, fillPriceFn: PriceQuoteFn): Boolean =
//    saleProceeds(order.time, order.symbol, order.qty, trial.commissionPerTrade, trial.commissionPerShare, fillPriceFn) >= 0.0
//
//  def isLimitBuyFillable(trial: Trial, portfolio: Portfolio, order: LimitOrder, fillPriceFn: PriceQuoteFn): Boolean =
//    isMarketBuyFillable(trial, portfolio, order, fillPriceFn) && fillPriceFn(order.time, order.symbol) <= order.limitPrice
//
//  def isLimitSellFillable(trial: Trial, portfolio: Portfolio, order: LimitOrder, fillPriceFn: PriceQuoteFn): Boolean =
//    isMarketSellFillable(trial, portfolio, order, fillPriceFn) && fillPriceFn(order.time, order.symbol) >= order.limitPrice

  // this is what a type class looks like in Scala - ewwwwww - say hello to java-esque boilerplate and a bunch of stupid implicits
  trait IsOrderFillable[OrderType] {
    def isOrderFillable(order: OrderType, trial: Trial, portfolio: Portfolio, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Boolean
  }

  object IsOrderFillable {
    implicit object IsMarketBuyFillable extends IsOrderFillable[MarketBuy] {
      def isOrderFillable(order: MarketBuy, trial: Trial, portfolio: Portfolio, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Boolean =
        purchaseCost(order.time, order.symbol, order.qty, trial.commissionPerTrade, trial.commissionPerShare, purchaseFillPriceFn) <= portfolio.cash
    }

    implicit object IsMarketSellFillable extends IsOrderFillable[MarketSell] {
      def isOrderFillable(order: MarketSell, trial: Trial, portfolio: Portfolio, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Boolean =
        saleProceeds(order.time, order.symbol, order.qty, trial.commissionPerTrade, trial.commissionPerShare, saleFillPriceFn) >= 0.0
    }

    implicit object IsLimitBuyFillable extends IsOrderFillable[LimitBuy] {
      def isOrderFillable(order: LimitBuy, trial: Trial, portfolio: Portfolio, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Boolean =
        IsMarketBuyFillable.isOrderFillable(order.asInstanceOf[MarketBuy], trial, portfolio, purchaseFillPriceFn, saleFillPriceFn) && purchaseFillPriceFn(order.time, order.symbol) <= order.limitPrice
    }

    implicit object IsLimitSellFillable extends IsOrderFillable[LimitSell] {
      def isOrderFillable(order: LimitSell, trial: Trial, portfolio: Portfolio, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Boolean =
        IsMarketSellFillable.isOrderFillable(order.asInstanceOf[MarketSell], trial, portfolio, purchaseFillPriceFn, saleFillPriceFn) && saleFillPriceFn(order.time, order.symbol) >= order.limitPrice
    }
  }

  def isOrderFillable[OrderType](order: OrderType, trial: Trial, portfolio: Portfolio, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn)(implicit iof: IsOrderFillable[OrderType]): Boolean =
    iof.isOrderFillable(order, trial, portfolio, purchaseFillPriceFn, saleFillPriceFn)

//  def isOrderFillable(trial: Trial, portfolio: Portfolio, order: MarketBuy, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Boolean =
//    purchaseCost(order.time, order.symbol, order.qty, trial.commissionPerTrade, trial.commissionPerShare, purchaseFillPriceFn) <= portfolio.cash
//
//  def isOrderFillable(trial: Trial, portfolio: Portfolio, order: MarketSell, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Boolean =
//    saleProceeds(order.time, order.symbol, order.qty, trial.commissionPerTrade, trial.commissionPerShare, saleFillPriceFn) >= 0.0
//
//  def isOrderFillable(trial: Trial, portfolio: Portfolio, order: LimitBuy, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Boolean =
//    isOrderFillable(trial, portfolio, order.asInstanceOf[MarketBuy], purchaseFillPriceFn, saleFillPriceFn) && purchaseFillPriceFn(order.time, order.symbol) <= order.limitPrice
//
//  def isOrderFillable(trial: Trial, portfolio: Portfolio, order: LimitSell, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): Boolean =
//    isOrderFillable(trial, portfolio, order.asInstanceOf[MarketSell], purchaseFillPriceFn, saleFillPriceFn) && saleFillPriceFn(order.time, order.symbol) >= order.limitPrice

  def orderFillPrice(order: BuyOrder, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): BigDecimal = purchaseFillPriceFn(order.time, order.symbol)
  def orderFillPrice(order: SellOrder, purchaseFillPriceFn: PriceQuoteFn, saleFillPriceFn: PriceQuoteFn): BigDecimal = saleFillPriceFn(order.time, order.symbol)

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
