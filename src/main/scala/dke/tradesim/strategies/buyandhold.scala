package dke.tradesim.strategies

import org.joda.time.LocalTime
import dke.tradesim.adjustedQuotes.adjEodSimQuote
import dke.tradesim.core.{SecurityId, State, Trial, Strategy, defaultInitialState}
import dke.tradesim.datetimeUtils.{periodBetween, randomDateTime, datetime, years, days}
import dke.tradesim.logger._
import dke.tradesim.math.floor
import dke.tradesim.ordering.{maxSharesPurchasable, sharesOnHand, buy, sell}
import dke.tradesim.portfolio.portfolioValue
import dke.tradesim.quotes.{barHigh, barLow, barClose, barSimQuote, findEodBar}
import dke.tradesim.schedule.{buildTradingSchedule, defaultTradingSchedule, defaultHolidaySchedule}
import dke.tradesim.securities.{findStocks, PrimaryUsExchanges}
import dke.tradesim.trial.{buildScheduledTimeIncrementer, buildInitialJumpTimeIncrementer, tradingBloxFillPriceWithSlippage, runTrial, buildTrialGenerator, buildAllTrialIntervals, buildTrials, fixedTradingPeriodIsFinalState, runAndLogTrialsInParallel}
import dke.tradesim.securities

object buyandhold {
  def initialState(strategy: Strategy, trial: Trial): State = defaultInitialState(trial.startTime, trial.principal)

  def nextState(strategy: Strategy, trial: Trial, state: State): State = {
    val time = state.time
    val startTime = trial.startTime
    val endTime = trial.endTime
    val securityId = trial.securityIds.head
    val portfolio = state.portfolio

    time match {
      case _ if time == startTime =>
        val qty = floor(maxSharesPurchasable(trial, portfolio, time, securityId, adjEodSimQuote).getOrElse(0)).toLong
        buy(state, time, securityId, qty)
      case _ if time == endTime =>
        sell(state, time, securityId, sharesOnHand(portfolio, securityId))
      case _ => state
    }
  }

  def buildStrategy(): Strategy = Strategy("Buy And Hold", initialState, nextState, fixedTradingPeriodIsFinalState)

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
      val securityIds = findStocks(PrimaryUsExchanges, Seq("MSFT")).flatMap(_.id).toVector
      val trial = Trial(securityIds, 10000, 7.0, 0.0, startTime, endTime, timeIncrementerFn, purchaseFillPriceFn, saleFillPriceFn)
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
      val securityIds = findStocks(PrimaryUsExchanges, Seq("MSFT")).flatMap(_.id).toVector
      val trial = Trial(securityIds, 10000, 7.0, 0.0, startTime, endTime, timeIncrementerFn, purchaseFillPriceFn, saleFillPriceFn)
      val finalState = runTrial(strategy, trial)
      val finalPortfolioValue = portfolioValue(finalState.portfolio, endTime, barClose _, barSimQuote _)
      println("finalState")
      println(finalState)
      println("finalPortfolioValue")
      println(finalPortfolioValue)
    }

    def runMultipleTrials1() {
      val tradingSchedule = buildTradingSchedule(defaultTradingSchedule, defaultHolidaySchedule)
//      val timeIncrementerFn = buildInitialJumpTimeIncrementer(new LocalTime(12, 0, 0), years(1), days(1), tradingSchedule)
      val timeIncrementerFn = buildScheduledTimeIncrementer(new LocalTime(12, 0, 0), days(1), tradingSchedule)
      val purchaseFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barHigh _, 0.15)
      val saleFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barLow _, 0.15)
      val strategy = buildStrategy()
      val trialGenerator = buildTrialGenerator(10000, 7.0, 0.0, timeIncrementerFn, purchaseFillPriceFn, saleFillPriceFn)
      val securityIds = findStocks(PrimaryUsExchanges, Seq("AAPL")).flatMap(_.id).toVector
      val trialIntervalBuilderFn = buildAllTrialIntervals(_: IndexedSeq[SecurityId], years(1), days(1))
      info("Building trials")
      val trials = buildTrials(strategy, trialIntervalBuilderFn, trialGenerator, securityIds)
      info(s"${trials.length} trials")
      runAndLogTrialsInParallel(strategy, trials)
    }

    def runMultipleTrials2() {
      val tradingSchedule = buildTradingSchedule(defaultTradingSchedule, defaultHolidaySchedule)
//      val timeIncrementerFn = buildInitialJumpTimeIncrementer(new LocalTime(12, 0, 0), years(1), days(1), tradingSchedule)
      val timeIncrementerFn = buildScheduledTimeIncrementer(new LocalTime(12, 0, 0), days(1), tradingSchedule)
      val purchaseFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barHigh _, 0.15)
      val saleFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barLow _, 0.15)
      val strategy = buildStrategy()
      val trialGenerator = buildTrialGenerator(10000, 7.0, 0.0, timeIncrementerFn, purchaseFillPriceFn, saleFillPriceFn)
      val securityIds = findStocks(PrimaryUsExchanges, Seq("AAPL")).flatMap(_.id).toVector
      val trialIntervalBuilderFn = buildAllTrialIntervals(_: IndexedSeq[SecurityId], years(1), days(1))
      val trials = buildTrials(strategy, trialIntervalBuilderFn, trialGenerator, securityIds)
      info(s"${trials.length} trials")
      runAndLogTrialsInParallel(strategy, trials)
    }

  }
}