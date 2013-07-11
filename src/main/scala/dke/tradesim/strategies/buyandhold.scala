package dke.tradesim.strategies

import org.joda.time.{LocalTime}
import dke.tradesim.adjustedQuotes.{adjEodSimQuote}
import dke.tradesim.core.{State, Trial, Strategy, defaultInitialState}
import dke.tradesim.datetimeUtils.{periodBetween, randomDateTime, datetime, years, days, currentTime, prettyFormatPeriod}
import dke.tradesim.math.{floor}
import dke.tradesim.ordering.{maxSharesPurchasable, sharesOnHand, buy, sell}
import dke.tradesim.portfolio.{portfolioValue}
import dke.tradesim.quotes.{barHigh, barLow, barClose, barSimQuote, findEodBar}
import dke.tradesim.schedule.{buildTradingSchedule, defaultTradingSchedule, defaultHolidaySchedule}
import dke.tradesim.trial.{buildScheduledTimeIncrementer, buildInitialJumpTimeIncrementer, tradingBloxFillPriceWithSlippage, runTrial, buildTrialGenerator, buildAllTrialIntervals, buildTrials, runTrials, runTrialsInParallel, fixedTradingPeriodIsFinalState}

object buyandhold {
  def initialState(strategy: Strategy, trial: Trial): State = defaultInitialState(trial.startTime, trial.principal)

  def nextState(strategy: Strategy, trial: Trial, state: State): State = {
    val time = state.time
    val startTime = trial.startTime
    val endTime = trial.endTime
    val symbol = trial.symbols.head
    val portfolio = state.portfolio

    time match {
      case _ if time == startTime =>
        val qty = floor(maxSharesPurchasable(trial, portfolio, time, symbol, adjEodSimQuote).getOrElse(0)).toLong
        buy(state, time, symbol, qty)
      case _ if time == endTime =>
        sell(state, time, symbol, sharesOnHand(portfolio, symbol))
      case _ => state
    }
  }

  def buildStrategy(): Strategy = Strategy(initialState, nextState, fixedTradingPeriodIsFinalState)

  object scenarios {
    def runSingleTrial1() {
      val tradingPeriodStart = randomDateTime(datetime(2000, 1, 1), datetime(2009, 1, 1))
      val startTime = datetime(2003, 2, 15, 12, 0, 0)
      val endTime = datetime(2003, 2, 25, 12, 0, 0)
      val tradingSchedule = buildTradingSchedule(defaultTradingSchedule _, defaultHolidaySchedule _)
      //    val timeIncrementerFn = buildScheduledTimeIncrementer(new LocalTime(12, 0, 0), days(1), tradingSchedule)
      val timeIncrementerFn = buildInitialJumpTimeIncrementer(new LocalTime(12, 0, 0), periodBetween(startTime, endTime), days(1), tradingSchedule)
      val purchaseFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barHigh _, 0.15)
      val saleFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barLow _, 0.15)
      val strategy = buildStrategy()
      val trial = Trial(Vector("MSFT"), 10000, 7.0, 0.0, startTime, endTime, timeIncrementerFn, purchaseFillPriceFn, saleFillPriceFn)
      val finalState = runTrial(strategy, trial)
      val finalPortfolioValue = portfolioValue(finalState.portfolio, endTime, barClose _, barSimQuote _)
      println("finalState")
      println(finalState)
      println("finalPortfolioValue")
      println(finalPortfolioValue)
    }

    def runSingleTrial2() {
      val tradingPeriodStart = randomDateTime(datetime(2000, 1, 1), datetime(2009, 1, 1))
      val startTime = datetime(2003, 2, 15, 12, 0, 0)
      val endTime = datetime(2003, 2, 25, 12, 0, 0)
      val tradingSchedule = buildTradingSchedule(defaultTradingSchedule _, defaultHolidaySchedule _)
      val timeIncrementerFn = buildScheduledTimeIncrementer(new LocalTime(12, 0, 0), days(1), tradingSchedule)
//      val timeIncrementerFn = buildInitialJumpTimeIncrementer(new LocalTime(12, 0, 0), periodBetween(startTime, endTime), days(1), tradingSchedule)
      val purchaseFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barHigh _, 0.15)
      val saleFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barLow _, 0.15)
      val strategy = buildStrategy()
      val trial = Trial(Vector("MSFT"), 10000, 7.0, 0.0, startTime, endTime, timeIncrementerFn, purchaseFillPriceFn, saleFillPriceFn)
      val finalState = runTrial(strategy, trial)
      val finalPortfolioValue = portfolioValue(finalState.portfolio, endTime, barClose _, barSimQuote _)
      println("finalState")
      println(finalState)
      println("finalPortfolioValue")
      println(finalPortfolioValue)
    }

    def runMultipleTrials1() {
      val tradingSchedule = buildTradingSchedule(defaultTradingSchedule, defaultHolidaySchedule)
      val timeIncrementerFn = buildInitialJumpTimeIncrementer(new LocalTime(12, 0, 0), years(1), days(1), tradingSchedule)
      val purchaseFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barHigh _, 0.15)
      val saleFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barLow _, 0.15)
      val strategy = buildStrategy()
      val trialGenerator = buildTrialGenerator(10000, 7.0, 0.0, timeIncrementerFn, purchaseFillPriceFn, saleFillPriceFn)
      val symbolsToTrade = Vector("AAPL")
      val trialIntervalBuilderFn = buildAllTrialIntervals(_: IndexedSeq[String], years(1), days(1))
      println("Building trials")
      val trials = buildTrials(strategy, trialIntervalBuilderFn, trialGenerator, symbolsToTrade)
      println(s"${trials.length} trials")
      val t1 = currentTime()
      val finalTrialStates = runTrials(strategy, trials)
      val t2 = currentTime()
      println(s"Time: ${prettyFormatPeriod(periodBetween(t1, t2))}")
      val finalPortfolioValues = finalTrialStates.zip(trials).map { pair =>
        val (state, trial) = pair
        portfolioValue(state.portfolio, trial.endTime, barClose _, barSimQuote _)
      }
      println(finalPortfolioValues)
    }

    def runMultipleTrials2() {
      val tradingSchedule = buildTradingSchedule(defaultTradingSchedule, defaultHolidaySchedule)
      val timeIncrementerFn = buildInitialJumpTimeIncrementer(new LocalTime(12, 0, 0), years(1), days(1), tradingSchedule)
      val purchaseFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barHigh _, 0.15)
      val saleFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barLow _, 0.15)
      val strategy = buildStrategy()
      val trialGenerator = buildTrialGenerator(10000, 7.0, 0.0, timeIncrementerFn, purchaseFillPriceFn, saleFillPriceFn)
      val symbolsToTrade = Vector("AAPL")
      val trialIntervalBuilderFn = buildAllTrialIntervals(_: IndexedSeq[String], years(1), days(1))
      val trials = buildTrials(strategy, trialIntervalBuilderFn, trialGenerator, symbolsToTrade)
      println(s"${trials.length} trials")
      val t1 = currentTime()
      val finalTrialStates = runTrialsInParallel(strategy, trials)
      val t2 = currentTime()
      println(s"Time: ${prettyFormatPeriod(periodBetween(t1, t2))}")
      val finalPortfolioValues = finalTrialStates.zip(trials).map { pair =>
        val (state, trial) = pair
        portfolioValue(state.portfolio, trial.endTime, barClose _, barSimQuote _)
      }
      println(finalPortfolioValues)
    }

  }
}