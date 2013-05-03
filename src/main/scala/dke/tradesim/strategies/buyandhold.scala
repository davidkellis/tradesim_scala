package dke.tradesim.strategies

import org.joda.time.LocalTime
import dke.tradesim.adjustedQuotes.{adjEodSimQuote}
import dke.tradesim.core.{State, Trial, Strategy, defaultInitialState}
import dke.tradesim.datetimeUtils.{randomDateTime, datetime, years, days}
import dke.tradesim.math.{floor}
import dke.tradesim.ordering.{maxSharesPurchasable, sharesOnHand, buy, sell}
import dke.tradesim.portfolio.{portfolioValue}
import dke.tradesim.quotes.{barHigh, barLow, barClose, barSimQuote, findEodBar}
import dke.tradesim.schedule.{buildTradingSchedule, defaultTradingSchedule, defaultHolidaySchedule}
import dke.tradesim.trial.{buildScheduledTimeIncrementer, tradingBloxFillPriceWithSlippage, runTrial, buildTrialGenerator, buildAllTrialIntervals, buildTrials, runTrials, fixedTradingPeriodIsFinalState}

object buyandhold {
  def initialState(strategy: Strategy, trial: Trial): State = defaultInitialState(trial.startTime, trial.principal)

  def nextState(strategy: Strategy, trial: Trial, state: State): State = {
    val time = state.time
    val startTime = trial.startTime
    val endTime = trial.endTime
    val symbol = trial.symbols.head
    val portfolio = state.portfolio

    time match {
      case startTime =>
        val qty = floor(maxSharesPurchasable(trial, portfolio, time, symbol, adjEodSimQuote).getOrElse(0)).toLong
        buy(state, time, symbol, qty)
      case endTime =>
        sell(state, time, symbol, sharesOnHand(portfolio, symbol))
      case _ => state
    }
  }

  def buildStrategy(): Strategy = Strategy(initialState, nextState, fixedTradingPeriodIsFinalState)

  def runSingleTrial() {
    val tradingPeriodStart = randomDateTime(datetime(2000, 1, 1), datetime(2009, 1, 1))
    val startTime = datetime(2012, 8, 1, 12, 0, 0)
    val endTime = datetime(2012, 8, 15, 12, 0, 0)
    val tradingSchedule = buildTradingSchedule(defaultTradingSchedule _, defaultHolidaySchedule _)
    val timeIncrementerFn = buildScheduledTimeIncrementer(new LocalTime(12, 0, 0), days(1), tradingSchedule)
    val purchaseFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barHigh _, 0.15)
    val saleFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barLow _, 0.15)
    val strategy = buildStrategy()
    val trial = Trial(Vector("AAPL"), 10000, 7.0, 0.0, startTime, endTime, timeIncrementerFn, purchaseFillPriceFn, saleFillPriceFn)
    val finalState = runTrial(strategy, trial)
    val finalPortfolioValue = portfolioValue(finalState.portfolio, endTime, barClose _, barSimQuote _)
    println(finalState)
    println(finalPortfolioValue)
  }

  def runMultipleTrials() {
    val tradingSchedule = buildTradingSchedule(defaultTradingSchedule, defaultHolidaySchedule)
    val timeIncrementerFn = buildScheduledTimeIncrementer(new LocalTime(12, 0, 0), days(1), tradingSchedule)
    val purchaseFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barHigh _, 0.15)
    val saleFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barLow _, 0.15)
    val strategy = buildStrategy()
    val trialGenerator = buildTrialGenerator(10000, 7.0, 0.0, timeIncrementerFn, purchaseFillPriceFn, saleFillPriceFn)
    val symbolsToTrade = Vector("AAPL")
    val trialIntervalBuilderFn = buildAllTrialIntervals(_: IndexedSeq[String], years(1), days(1))
    val trials = buildTrials(strategy, trialIntervalBuilderFn, trialGenerator, symbolsToTrade)
    val finalTrialStates = runTrials(strategy, trials)
    val finalPortfolioValues = finalTrialStates.zip(trials).map { pair =>
      val (state, trial) = pair
      portfolioValue(state.portfolio, trial.endTime, barClose _, barSimQuote _)
    }
    println(finalPortfolioValues)
  }
}