package dke.tradesim

import dke.tradesim.core._
import dke.tradesim.datetime.{isAfterOrEqual}
import dke.tradesim.schedule.{TradingSchedule, nextTradingDay}
import dke.tradesim.ordering.{isOrderFillable, orderFillPrice}
import dke.tradesim.quotes.{barSimQuote}
import org.joda.time.{DateTime, Period, ReadablePartial}
import dke.tradesim.core.State
import dke.tradesim.core.Trial
import dke.tradesim.core.Strategy

object trial {
  def fixedTradingPeriodIsFinalState(strategy: Strategy, trial: Trial, state: State): Boolean = isAfterOrEqual(state.time, trial.endTime)

  /**
   * Returns a function of one argument, <time>. The function returns the next scheduled time after <time> such that the time
   * component of the new time is the same time specified in <time-component> and the date component of the new time is the soonest
   * day in the <trading-schedule> that follows the date component of <time>, and contains the <time-component>.
   */
  def buildScheduledTimeIncrementer(timeComponent: ReadablePartial, periodIncrement: Period, tradingSchedule: TradingSchedule): (DateTime) => DateTime = {
    (time: DateTime) => {
      val nextDay = nextTradingDay(time.toLocalDate, tradingSchedule).toDateTimeAtStartOfDay(time.getZone)
      timeComponent.toDateTime(nextDay)
    }
  }

  /**
   * Returns a function of <time> and <symbol>, that when invoked returns the price at which a trade of <symbol> would have been
   * filled in the market as of <time>.
   * The simulated fill-price is adjusted for splits/dividends.
   * slippage is a percentage expressed as a real number in the interval (-1, 1) to skew the base quote
   *   up or down depending on the sign of slippage. If slippage is positive, then the base quote is skewed higher (toward +infinity)
   *   and if the slippage is negative, then the base price is skewed lower (toward -infinity).
   *   Example: if the slippage amount is a +3%, then slippage should be given as 0.03.
   *            if the slippage amount is a -4%, then slippage should be given as -0.04.
   */
  def naiveFillPriceWithSlippage(priceBarFn: (DateTime, String) => Bar, slippage: BigDecimal): (DateTime, String) => BigDecimal = {
    val slippageMultiplier = 1 + slippage
    (time: DateTime, symbol: String) => {
      val bar = priceBarFn(time, symbol)
      val fillPrice = barSimQuote(bar) * slippageMultiplier
      adjustPriceForCorporateActions(fillPrice, symbol, bar.endTime, time)
    }
  }

  /**
   * Returns a function of <time> and <symbol>, that when invoked returns the price at which a trade of <symbol> would have been
   * filled in the market as of <time>.The simulated fill-price is adjusted for splits/dividends.
   * Arguments:
   * order-price-fn is a function of an OHLC-bar (e.g. (bar-close OHLC-bar))
   * price-bar-extremum-fn is a function of an OHLC-bar and should be either bar-high or bar-low (i.e. high or low)
   * slippage is a percentage expressed as a real number in the interval [0, 1). It is never negative.
   * Found the formulas for this fill-price estimation technique from
   *   http://www.automated-trading-system.com/slippage-backtesting-realistic/
   * The formula is:
   *   order-price + slippage-multiplier * ([high|low] - order-price)
   */
  def tradingBloxFillPriceWithSlippage(priceBarFn: (DateTime, String) => Bar,
                                       orderPriceFn: (Bar) => BigDecimal,
                                       priceBarExtremumFn: (Bar) => BigDecimal,
                                       slippage: BigDecimal): BigDecimal = {
    (time: DateTime, symbol: String) => {
      val bar = priceBarFn(time, symbol)
      val orderPrice = orderPriceFn(bar)
      val fillPrice = orderPrice + slippage * (priceBarExtremumFn(bar) - orderPrice)
      adjustPriceForCorporateActions(fillPrice, symbol, bar.endTime, time)
    }
  }

  /**
   * trial.purchase-fill-price is a function of <time> and <symbol> that returns the fill price of a buy order for <symbol> at <time>
   * trial.sale-fill-price is a function of <time> and <symbol> that returns the fill price of a sell order for <symbol> at <time>
   *
   * Returns a new current-state such that the new-current-state.orders may contain fewer open orders than
   * current-state.orders (meaning, orders may get filled); new-current-state.portfolio may contain
   * fewer or more shares/cash/etc. than current-state.portfolio (meaning, the portfolio will be adjusted for filled orders);
   * and new-current-state.transactions may contain additional filled orders (NOTE: filled orders have a non-nil fill-price)
   */
  def executeOrders(trial: Trial, currentState: State): State = {
    val purchaseFillPriceFn = trial.purchaseFillPrice
    val saleFillPriceFn = trial.saleFillPrice

    def executeOrders(portfolio: Portfolio, orders: Seq[Order], unfilledOrders: Seq[Order], transactions: Seq[Order]): State = {
      if (orders.isEmpty)
        currentState.copy(portfolio = portfolio, orders = unfilledOrders, transactions = transactions)
      else {
        val order = orders.head
        val nextOrders = orders.tail

        if (isOrderFillable(order, trial, portfolio, purchaseFillPriceFn, saleFillPriceFn)) {
          val fillPrice = orderFillPrice(order, purchaseFillPriceFn, saleFillPriceFn)
          val filledOrder = order.changeFillPrice(fillPrice)
        }
      }
    }

    executeOrders(currentState.portfolio, currentState.orders, Vector(), currentState.transactions)
  }
}