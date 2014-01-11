package dke.tradesim.strategies

import org.joda.time.{DateTime, LocalTime}
import dke.tradesim.adjustedQuotes.adjEodSimQuote
import dke.tradesim.core._
import dke.tradesim.datetimeUtils.{periodBetween, randomDateTime, datetime, years, days}
import dke.tradesim.logger._
import dke.tradesim.math.floor
import dke.tradesim.ordering.{maxSharesPurchasable, sharesOnHand, buy, sell}
import dke.tradesim.portfolio.portfolioValue
import dke.tradesim.quotes.{barHigh, barLow, barClose, barSimQuote, findEodBar}
import dke.tradesim.schedule.{buildTradingSchedule, defaultTradingSchedule, defaultHolidaySchedule}
import dke.tradesim.securities.{findSecurities, PrimaryUsExchanges}
import dke.tradesim.trial.{buildScheduledTimeIncrementer, buildInitialJumpTimeIncrementer, tradingBloxFillPriceWithSlippage, runTrial, buildTrialGenerator, buildAllTrialIntervals, buildTrials, fixedTradingPeriodIsFinalState, runAndLogTrialsInParallel}
import dke.tradesim.core
import dke.tradesim.core.Strategy
import dke.tradesim.core.Trial

object buyandhold {

  case class State(previousTime: DateTime,
                   time: DateTime,
                   portfolio: Portfolio,
                   orders: IndexedSeq[Order],
                   transactions: TransactionLog,
                   portfolioValueHistory: Seq[PortfolioValue],
                   hasEnteredPosition: Boolean) extends core.State[State] {

    def copy(previousTime: DateTime = previousTime,
             time: DateTime = time,
             portfolio: Portfolio = portfolio,
             orders: IndexedSeq[Order] = orders,
             transactions: TransactionLog = transactions,
             portfolioValueHistory: Seq[PortfolioValue] = portfolioValueHistory): State =
      new State(previousTime, time, portfolio, orders, transactions, portfolioValueHistory, hasEnteredPosition)
      
    def withHasEnteredPosition(hasEnteredPosition: Boolean): State = new State(previousTime, time, portfolio, orders, transactions, portfolioValueHistory, hasEnteredPosition)
  }

  def initialState(strategy: Strategy[State], trial: Trial): State = State(null, null, null, null, null, null, false).initializeState(trial.startTime, trial.principal)

  def nextState(strategy: Strategy[State], trial: Trial, state: State): State = {
    val time = state.time
    val endTime = trial.endTime
    val securityId = trial.securityIds.head
    val portfolio = state.portfolio

    if (!state.hasEnteredPosition) {
      val qty = floor(maxSharesPurchasable(trial, portfolio, time, securityId, adjEodSimQuote _).getOrElse(0)).toLong
//      info(s"buy $qty shares")
//      info(s"state = $state")
      val newState = buy(state, time, securityId, qty).withHasEnteredPosition(true)
//      info(s"newState = $newState")
      newState
    } else if (time == endTime) {
//      info(s"sell all shares")
      sell(state, time, securityId, sharesOnHand(portfolio, securityId))
    } else {
      state
    }
  }

  def buildStrategy(): Strategy[State] = Strategy("Buy And Hold", initialState, nextState, fixedTradingPeriodIsFinalState)

  object scenarios {
    def runSingleTrial1() {
//      val tradingPeriodStart = randomDateTime(datetime(2000, 1, 1), datetime(2009, 1, 1))
      val startTime = datetime(2003, 2, 15, 12, 0, 0)
      val endTime = datetime(2004, 2, 25, 12, 0, 0)
      val tradingSchedule = buildTradingSchedule(defaultTradingSchedule _, defaultHolidaySchedule _)
      val timeIncrementerFn = buildScheduledTimeIncrementer(new LocalTime(12, 0, 0), days(1), tradingSchedule)
//      val timeIncrementerFn = buildInitialJumpTimeIncrementer(new LocalTime(12, 0, 0), periodBetween(startTime, endTime), days(1), tradingSchedule)
      val purchaseFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barHigh _, 0.15)
      val saleFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barLow _, 0.15)
      val strategy = buildStrategy()
      val securityIds = findSecurities(PrimaryUsExchanges, Seq("AAPL")).flatMap(_.id).toVector
      val trial = Trial(securityIds, 10000, 7.0, 0.0, startTime, endTime, timeIncrementerFn, purchaseFillPriceFn, saleFillPriceFn)
      info("Running 1 trial")
      runAndLogTrialsInParallel(strategy, Seq(trial))
    }

    def runSingleTrial2() {
//      val tradingPeriodStart = randomDateTime(datetime(2000, 1, 1), datetime(2009, 1, 1))
      val startTime = datetime(2003, 2, 15, 12, 0, 0)
      val endTime = datetime(2004, 2, 25, 12, 0, 0)
      val tradingSchedule = buildTradingSchedule(defaultTradingSchedule _, defaultHolidaySchedule _)
      val timeIncrementerFn = buildScheduledTimeIncrementer(new LocalTime(12, 0, 0), days(1), tradingSchedule)
//      val timeIncrementerFn = buildInitialJumpTimeIncrementer(new LocalTime(12, 0, 0), periodBetween(startTime, endTime), days(1), tradingSchedule)
      val purchaseFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barHigh _, 0.15)
      val saleFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barLow _, 0.15)
      val strategy = buildStrategy()
      val securityIds = findSecurities(PrimaryUsExchanges, Seq("AAPL")).flatMap(_.id).toVector
      val trial = Trial(securityIds, 10000, 7.0, 0.0, startTime, endTime, timeIncrementerFn, purchaseFillPriceFn, saleFillPriceFn)
      info("Running 1 trial")
      runAndLogTrialsInParallel(strategy, Seq(trial))
    }

    def runMultipleTrials1() {
      val tradingSchedule = buildTradingSchedule(defaultTradingSchedule, defaultHolidaySchedule)
//      val timeIncrementerFn = buildInitialJumpTimeIncrementer(new LocalTime(12, 0, 0), years(1), days(1), tradingSchedule)
      val timeIncrementerFn = buildScheduledTimeIncrementer(new LocalTime(12, 0, 0), days(1), tradingSchedule)
      val purchaseFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barHigh _, 0.15)
      val saleFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barLow _, 0.15)
      val strategy = buildStrategy()
      val trialGenerator = buildTrialGenerator(10000, 0.0, 7.0, timeIncrementerFn, purchaseFillPriceFn, saleFillPriceFn)
      val securityIds = findSecurities(PrimaryUsExchanges, Seq("IJH")).flatMap(_.id).toVector
      val trialIntervalBuilderFn = buildAllTrialIntervals(_: IndexedSeq[SecurityId], years(1), days(1))  //.take(500)
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
      val trialGenerator = buildTrialGenerator(10000, 0.0, 7.0, timeIncrementerFn, purchaseFillPriceFn, saleFillPriceFn)
      val securityIds = findSecurities(PrimaryUsExchanges, Seq("AAPL")).flatMap(_.id).toVector
      val trialIntervalBuilderFn = buildAllTrialIntervals(_: IndexedSeq[SecurityId], years(1), days(1))
      val trials = buildTrials(strategy, trialIntervalBuilderFn, trialGenerator, securityIds)
      info(s"${trials.length} trials")
      runAndLogTrialsInParallel(strategy, trials)
    }

    def runMultipleTrials3() {
      val tradingSchedule = buildTradingSchedule(defaultTradingSchedule, defaultHolidaySchedule)
      val timeIncrementerFn = buildScheduledTimeIncrementer(new LocalTime(12, 0, 0), days(1), tradingSchedule)
      val purchaseFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barHigh _, 0.15)
      val saleFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barLow _, 0.15)
      val strategy = buildStrategy()
      val trialGenerator = buildTrialGenerator(10000, 0.0, 7.0, timeIncrementerFn, purchaseFillPriceFn, saleFillPriceFn)
      val trialIntervalBuilderFn = buildAllTrialIntervals(_: IndexedSeq[SecurityId], years(1), days(1))     //.take(500)

      val securities = findSecurities(PrimaryUsExchanges, Seq("ACITX","PTRRX","FKSRX","PHYRX","ACVAX","ACCAX","ACOAX","RELDX","SSAIX","MRLOX","REACX","GITSX","IENAX","GGHCX","ARFAX","ARBMX","ARWAX","ARCMX","ARYAX","ARDMX","AROAX","ARFMX","ALPAX","LCEAX","RRFDX","SVSPX","RGACX","JDCRX","RRGSX","MRVEX","IJH","TWVAX","RRMGX","FVFRX","ASQAX","SESPX","GTSRX","ODVNX","RERCX"))
      securities.foreach { security =>
        info("Building trials")
        val trialSecurityIds = Vector(security.id.get)
        val trials = buildTrials(strategy, trialIntervalBuilderFn, trialGenerator, trialSecurityIds)
        info(s"${trials.length} trials")
        runAndLogTrialsInParallel(strategy, trials)
      }
    }

  }
}