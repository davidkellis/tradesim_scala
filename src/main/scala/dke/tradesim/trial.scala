package dke.tradesim

import dke.tradesim.core._
import dke.tradesim.datetime.{isAfterOrEqual, interspersedIntervals}
import dke.tradesim.schedule.{TradingSchedule, nextTradingDay}
import dke.tradesim.ordering.{isOrderFillable, orderFillPrice, adjustPortfolioFromFilledOrder}
import dke.tradesim.quotes.{barSimQuote}
import org.joda.time.{Interval, DateTime, Period, ReadablePartial}
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
      if (orders.isEmpty)                                                                     // if there aren't any open orders...
        currentState.copy(portfolio = portfolio,                                              // return the new/next current state
                          orders = unfilledOrders,
                          transactions = transactions)
      else {                                                                                  // otherwise, try to fill the first open order:
        val order = orders.head
        val nextOrders = orders.tail

        if (isOrderFillable(order, trial, portfolio, purchaseFillPriceFn, saleFillPriceFn)) { // if the order is fillable, then fill it, and continue
          val fillPrice = orderFillPrice(order, purchaseFillPriceFn, saleFillPriceFn)
          val filledOrder = order.changeFillPrice(fillPrice)
          val nextPortfolio = adjustPortfolioFromFilledOrder(trial, portfolio, filledOrder)
          val nextTransactions = transactions :+ filledOrder

          executeOrders(nextPortfolio, nextOrders, unfilledOrders, nextTransactions)
        } else {                                                                              // otherwise, don't fill it, and continue
          executeOrders(portfolio, nextOrders, unfilledOrders :+ order, transactions)
        }
      }
    }

    executeOrders(currentState.portfolio, currentState.orders, Vector(), currentState.transactions)
  }

  /**
   * Returns a new State that has been adjusted for stock splits and dividend payouts that have gone into effect at some point within the
   * interval [current-state.previous-time, current-state.time].
   */
  def adjustStrategyStateForRecentSplitsAndDividends(currentState: State): State = {
    val portfolio = currentState.portfolio
    val openOrders = currentState.orders
    val previousTime = currentState.previousTime
    val currentTime = currentState.time
    val adjustedPortfolio = adjustPortfolioForCorporateActions(portfolio, previousTime, currentTime)
    val adjustedOpenOrders = adjustOpenOrdersForCorporateActions(openOrders, previousTime, currentTime)
    currentState.copy(portfolio = adjustedPortfolio, orders = adjustedOpenOrders)
  }

  def incrementStateTime(nextTime: DateTime, currentState: State): State = currentState.copy(previousTime = currentState.time, time = nextTime)

  def threadThrough[T](o: T, fns: (T => T)*): T = fns.foldLeft(o)((intermediateObject, transform) => transform(intermediateObject))

  def closeAllOpenPositions(trial: Trial, currentState: State): State =
    threadThrough(currentState, cancelAllPendingOrders, closeAllOpenStockPositions, executeOrders(trial, _))

  /*
   * This function runs a single trial.
   *
   * strategy is a Strategy object
   *   strategy.build-next-state is a function of <strategy>, <trial>, <state> that returns state' such that state' is
   *     the state of the strategy/trial immediately following the evaluation of the trading rules at time state.time.
   *     In other words, each invocation of build-next-state represents the opportunity for the trading strategy to
   *     evaluate its trading rules and submit one or more buy/sell orders at time state.time. That's all the strategy
   *     can do - submit buy/sell orders at time state.time. The *ONLY* difference between state and state' should be
   *     the list of open orders (which are stored in state.orders/state'.orders), and changes in state that have to
   *     do with the strategy's trading logic (e.g. a state machine or moving average computations, etc.).
   * trial is a Trial record
   *
   * Returns the final state that the strategy was in when the trial run completed.
   * NOTE: the trial's time incrementer function, :increment-time, ought to increment the time in such a way as to
   *   ensure that the time of the final state is very close or preferably equal to the time at which the trial is supposed/expected to end.
   *   Otherwise, if the trial is expected to end on Jan 1, 2010, but the time incrementer function increments by 6-month
   *   intervals, the final state may unexpectedly be June 1, 2010, which could be "bad" because the final state will have
   *   been adjusted for corporate actions that took place between Jan 1, 2010 and June 1, 2010, but the user
   *   might only expect the final state to have been adjusted for corporate actions before Jan 1, 2010.
   */
  def runTrial(strategy: Strategy, trial: Trial): State = {
    val buildInitStrategyState = strategy.buildInitState
    val buildNextStrategyState = strategy.buildNextState
    val isFinalState = strategy.isFinalState
    val incrementTime = trial.incrementTime

    def runTrial(currentState: State): State = {
      println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
      println("strategy=" + strategy)
      println("trial=" + trial)
      println("currentState=" + currentState)

      if (isFinalState(strategy, trial, currentState)) closeAllOpenPositions(trial, currentState)
      else {
        val currentTime = currentState.time
        val nextTime = incrementTime(currentTime)

        runTrial(threadThrough(currentState,
                               buildNextStrategyState(strategy, trial, _),
                               //TODO: should we increment state.time by 100 milliseconds here to represent the time between order entry and order execution?
                               executeOrders(trial, _),
                               incrementStateTime(nextTime, _),
                               adjustStrategyStateForRecentSplitsAndDividends))
      }
    }

    runTrial(buildInitStrategyState(strategy, trial))
  }

  // TODO: make sure this implementation is correct - the clojure version was wrong
  def buildAllTrialIntervals(symbolList: Seq[String], intervalLength: Period, separationLength: Period): Seq[Interval] = {
    val startDateRange = commonTrialPeriodStartDates(symbolList, intervalLength)
    val startDate = startDateRange.getStart
    interspersedIntervals(startDate, intervalLength, separationLength)
  }

  type TrialGenerator = (Seq[String], DateTime, DateTime) => Trial

  def buildTrialGenerator(principal: BigDecimal,
                          commissionPerTrade: BigDecimal,
                          commissionPerShare: BigDecimal,
                          timeIncrementerFn: (DateTime) => DateTime,
                          purchaseFillPriceFn: PriceQuoteFn,
                          saleFillPriceFn: PriceQuoteFn): TrialGenerator =
    (symbolList: Seq[String], startTime: DateTime, endTime: DateTime) => Trial(symbolList,
                                                                               principal,
                                                                               commissionPerShare,
                                                                               commissionPerTrade,
                                                                               startTime,
                                                                               endTime,
                                                                               timeIncrementerFn,
                                                                               purchaseFillPriceFn,
                                                                               saleFillPriceFn)

  def buildTrials(strategy: Strategy,
                  trialIntervalGeneratorFn: (Seq[String]) => Seq[Interval],
                  trialGeneratorFn: TrialGenerator,
                  symbolList: Seq[String]): Seq[Trial] = {
    val trialIntervals = trialIntervalGeneratorFn(symbolList)
    trialIntervals.map(interval => trialGeneratorFn(symbolList, interval.getStart, interval.getEnd))
  }

  def runTrials(strategy: Strategy, trials: Seq[Trial]): Seq[State] = trials.par.map(runTrial(strategy, _)).seq
}