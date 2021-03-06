package dke.tradesim.strategies

import org.joda.time.{Period, DateTime, LocalTime}
import com.github.nscala_time.time.Imports._
import dke.tradesim.adjustedQuotes.adjEodSimQuote
import dke.tradesim.core._
import dke.tradesim.datetimeUtils._
import dke.tradesim.logger._
import dke.tradesim.math.floor
import dke.tradesim.ordering._
import dke.tradesim.portfolio.portfolioValue
import dke.tradesim.quotes.{barHigh, barLow, barClose, barSimQuote, findEodBar}
import dke.tradesim.schedule._
import dke.tradesim.securities.{findSecurities, PrimaryUsExchanges}
import dke.tradesim.trial.{buildScheduledTimeIncrementer, buildInitialJumpTimeIncrementer, tradingBloxFillPriceWithSlippage, runTrial, buildTrialGenerator, buildAllTrialIntervals, buildTrials, fixedTradingPeriodIsFinalState, runAndLogTrialsInParallel}
import dke.tradesim.{core}
import dke.tradesim.core.Strategy
import dke.tradesim.core.Trial
import dke.tradesim.fundamentals.mrq
import dke.tradesim.screening._
import dke.tradesim.core.Portfolio
import dke.tradesim.core.PortfolioValue
import dke.tradesim.core.Strategy
import dke.tradesim.core.Trial
import dke.tradesim.core.Portfolio
import dke.tradesim.core.PortfolioValue
import dke.tradesim.core.Strategy
import dke.tradesim.core.Trial

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
    val securityIds = trial.securityIds
    val stateMachine = state.stateMachine

    stateMachine match {
      case NeedEnter =>
        val rankedSecurityIds = applyMagicFormulaRanking(securityIds, time, 50000000)
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

  def applyMagicFormulaRanking(securityIds: Seq[SecurityId], time: DateTime, marketCap: Long): IndexedSeq[SecurityId] = {
    val filteredSecurityIds = filter(securityIds) { securityId =>
      mrq.marketCapitalization(time, securityId).map(_ >= marketCap)
    }
    rank(filteredSecurityIds,
      Seq(Ordering.by { securityId: SecurityId => mrq.greenblattEarningsYield(time, securityId) },
          Ordering.by { securityId: SecurityId => mrq.greenblattReturnOnCapital(time, securityId) }))
  }

  def buildStrategy(): Strategy[State] = Strategy("Magic Formula", initialState, nextState, fixedTradingPeriodIsFinalState)

  object scenarios {
    def runSingleTrial1() {
      val trialDuration = years(1)
      val startTime = datetime(2003, 2, 15, 12, 0, 0)
      val endTime = startTime.plus(trialDuration)
      val tradingSchedule = buildTradingSchedule(defaultTradingSchedule _, defaultHolidaySchedule _)
      val timeIncrementerFn = buildScheduledTimeIncrementer(new LocalTime(12, 0, 0), days(1), tradingSchedule)
      val purchaseFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barHigh _, 0.3)
      val saleFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barLow _, 0.3)
      val strategy = buildStrategy()
      val securityIds = findSecurities(PrimaryUsExchanges, Seq("AAPL")).flatMap(_.id).toVector
      val trial = Trial(securityIds, 10000, 7.0, 0.0, startTime, endTime, trialDuration, timeIncrementerFn, purchaseFillPriceFn, saleFillPriceFn)
      info("Running 1 trial")
      runAndLogTrialsInParallel(strategy, Seq(trial))
    }

    def runMultipleTrials1() {
      val tradingSchedule = buildTradingSchedule(defaultTradingSchedule, defaultHolidaySchedule)
      val timeIncrementerFn = buildScheduledTimeIncrementer(new LocalTime(12, 0, 0), days(1), tradingSchedule)
      val purchaseFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barHigh _, 0.3)
      val saleFillPriceFn = tradingBloxFillPriceWithSlippage(findEodBar, barSimQuote _, barLow _, 0.3)
      val strategy = buildStrategy()
      val securityIds = findSecurities(PrimaryUsExchanges, Seq("AAPL")).flatMap(_.id).toVector
      val trialGenerator = buildTrialGenerator(10000, 0.0, 7.0, timeIncrementerFn, purchaseFillPriceFn, saleFillPriceFn)
      val trialIntervalBuilderFn = buildAllTrialIntervals(_: IndexedSeq[SecurityId], _: Period, days(1))
                                   .filter(interval => isTradingDay(date(interval.getStart), tradingSchedule))
      val trialDuration = years(1)
      val trials = buildTrials(strategy, trialIntervalBuilderFn, trialGenerator, securityIds, trialDuration)
      info(s"${trials.length} trials")
      runAndLogTrialsInParallel(strategy, trials)
    }
  }
}