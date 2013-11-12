package dke.tradesim.strategies

import org.joda.time.{DateTime, LocalTime}
import com.github.nscala_time.time.Imports._
import dke.tradesim.adjustedQuotes.adjEodSimQuote
import dke.tradesim.core._
import dke.tradesim.datetimeUtils.{intervalBetween, periodBetween, durationBetween, randomDateTime, datetime, years, days}
import dke.tradesim.logger._
import dke.tradesim.math.floor
import dke.tradesim.ordering._
import dke.tradesim.portfolio.portfolioValue
import dke.tradesim.quotes.{barHigh, barLow, barClose, barSimQuote, findEodBar}
import dke.tradesim.schedule.{buildTradingSchedule, defaultTradingSchedule, defaultHolidaySchedule}
import dke.tradesim.securities.{findStocks, PrimaryUsExchanges}
import dke.tradesim.trial.{buildScheduledTimeIncrementer, buildInitialJumpTimeIncrementer, tradingBloxFillPriceWithSlippage, runTrial, buildTrialGenerator, buildAllTrialIntervals, buildTrials, fixedTradingPeriodIsFinalState, runAndLogTrialsInParallel}
import dke.tradesim.{core}
import dke.tradesim.core.Strategy
import dke.tradesim.core.Trial
import dke.tradesim.fundamentals.mrq
import dke.tradesim.screening._

object magicformula {

  sealed abstract class StateMachine
  object NeedEnter extends StateMachine
  object Holding extends StateMachine

  case class State(previousTime: DateTime,
                   time: DateTime,
                   portfolio: Portfolio,
                   orders: IndexedSeq[Order],
                   transactions: TransactionLog,
                   portfolioValueHistory: Seq[PortfolioValue],
                   stateMachine: StateMachine,
                   lastPositionEntry: DateTime) extends core.State[State] {

    def copy(previousTime: DateTime = previousTime,
             time: DateTime = time,
             portfolio: Portfolio = portfolio,
             orders: IndexedSeq[Order] = orders,
             transactions: TransactionLog = transactions,
             portfolioValueHistory: Seq[PortfolioValue] = portfolioValueHistory): State =
      new State(previousTime, time, portfolio, orders, transactions, portfolioValueHistory, stateMachine, lastPositionEntry)
      
    def withStateMachine(stateMachine: StateMachine): State = new State(previousTime, time, portfolio, orders, transactions, portfolioValueHistory, stateMachine, lastPositionEntry)
    def withLastPositionEntry(lastPositionEntry: DateTime): State = new State(previousTime, time, portfolio, orders, transactions, portfolioValueHistory, stateMachine, lastPositionEntry)
  }

  def initialState(strategy: Strategy[State], trial: Trial): State =
    State(null, null, null, null, null, null, NeedEnter, datetime(1500, 1, 1)).initializeState(trial.startTime, trial.principal)

  def nextState(strategy: Strategy[State], trial: Trial, state: State): State = {
    val time = state.time
    val endTime = trial.endTime
    val securityIds = trial.securityIds
    val portfolio = state.portfolio
    val stateMachine = state.stateMachine

    stateMachine match {
      case NeedEnter =>
        val filteredSecurityIds = filter(securityIds) { securityId =>
          mrq.marketCapitalization(time, securityId).map(_ >= 50000000)
        }
        val rankedSecurityIds = rank(filteredSecurityIds,
                                     Seq(Ordering.by { securityId: SecurityId => mrq.greenblattEarningsYield(time, securityId) },
                                         Ordering.by { securityId: SecurityId => mrq.greenblattReturnOnCapital(time, securityId) }))
        val securityIdsToBuy = rankedSecurityIds.take(20)
        buyEqually(trial, state, securityIdsToBuy, adjEodSimQuote).withStateMachine(Holding)
      case Holding =>
        val timeToRebalancePortfolio = durationBetween(state.lastPositionEntry, time) >= years(1).toStandardDuration
        if (timeToRebalancePortfolio) {
          closeAllOpenStockPositions(state).withStateMachine(NeedEnter)
        } else
          state
    }
  }

  def buildStrategy(): Strategy[State] = Strategy("Magic Formula", initialState, nextState, fixedTradingPeriodIsFinalState)

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
      val securityIds = findStocks(PrimaryUsExchanges, Seq("AAPL")).flatMap(_.id).toVector
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
      val securityIds = findStocks(PrimaryUsExchanges, Seq("AAPL")).flatMap(_.id).toVector
      val trialIntervalBuilderFn = buildAllTrialIntervals(_: IndexedSeq[SecurityId], years(1), days(1))  //.take(500)
      info("Building trials")
      val trials = buildTrials(strategy, trialIntervalBuilderFn, trialGenerator, securityIds)
      info(s"${trials.length} trials")
      runAndLogTrialsInParallel(strategy, trials)
    }
  }
}