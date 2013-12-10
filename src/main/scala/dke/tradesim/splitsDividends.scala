package dke.tradesim

import scala.collection.JavaConversions._
import java.util.{NavigableMap, TreeMap}
import org.joda.time.DateTime
import net.sf.ehcache.{Element}

import dke.tradesim.datetimeUtils._
import dke.tradesim.core._
import dke.tradesim.db.{Adapter}
import dke.tradesim.logger._
import dke.tradesim.quotes.{findEodBarPriorTo, barClose}
import dke.tradesim.math.{floor}
import dke.tradesim.ordering.{sharesOnHand, setSharesOnHand, addCash, setOrderQty, setLimitPrice}

import Adapter.threadLocalAdapter

object splitsDividends {
  case class AdjustmentFactor(corporateAction: CorporateAction, priorEodBar: Option[Bar], adjustmentFactor: BigDecimal)
  case class QtyAdjustmentFactor(corporateAction: CorporateAction, adjustmentFactor: BigDecimal)


  def queryCorporateActions(securityId: SecurityId)(implicit adapter: Adapter): IndexedSeq[CorporateAction] = queryCorporateActions(Vector(securityId))
  def queryCorporateActions(securityIds: IndexedSeq[Int])(implicit adapter: Adapter): IndexedSeq[CorporateAction] = {
    info(s"queryCorporateActions(${securityIds.mkString(",")})")
    adapter.queryCorporateActions(securityIds)
  }

  def queryCorporateActions(securityId: SecurityId, startTime: DateTime, endTime: DateTime)(implicit adapter: Adapter): IndexedSeq[CorporateAction] =
    queryCorporateActions(Vector(securityId), startTime, endTime)
  def queryCorporateActions(securityIds: IndexedSeq[Int], startTime: DateTime, endTime: DateTime)(implicit adapter: Adapter): IndexedSeq[CorporateAction] = {
    info(s"queryCorporateActions(${securityIds.mkString(",")}, $startTime, $endTime)")
    adapter.queryCorporateActions(securityIds, startTime, endTime)
  }

  type CorporateActionHistory = NavigableMap[Timestamp, CorporateAction]

  def loadCorporateActionHistory(securityId: SecurityId): CorporateActionHistory = {
//    println(s"loadCorporateActionHistory($symbol)")
    val corporateActions = queryCorporateActions(securityId)
    val corporateActionHistory = new TreeMap[Timestamp, CorporateAction]()
    for {corporateAction <- corporateActions} corporateActionHistory.put(timestamp(corporateAction.exDate), corporateAction)
    corporateActionHistory
  }


  val corporateActionCache = cache.buildLruCache(32, "corporateActionCache")

  def findCorporateActionHistory(securityId: SecurityId): CorporateActionHistory = {
    val corporateActionHistory = Option(corporateActionCache.get(securityId))
    corporateActionHistory match {
      case Some(corporateActionHistoryElement) => corporateActionHistoryElement.getObjectValue.asInstanceOf[CorporateActionHistory]
      case None =>
        val newCorporateActionHistory = loadCorporateActionHistory(securityId)
        corporateActionCache.put(new Element(securityId, newCorporateActionHistory))
        newCorporateActionHistory
    }
  }

  def findCorporateActionsFromHistory(history: CorporateActionHistory, startTime: DateTime, endTime: DateTime): IndexedSeq[CorporateAction] = {
    val startTimestamp = timestamp(startTime)
    val endTimestamp = timestamp(endTime)
    val subHistory = history.subMap(startTimestamp, true, endTimestamp, true)
    val corporateActions = subHistory.values()
//    println(s"findCorporateActionsFromHistory(history, $startTime, $endTime) -> ${corporateActions.toVector}")
    corporateActions.toVector   // calls #toVector by implicit conversion
  }

  def findCorporateActions(securityId: SecurityId, startTime: DateTime, endTime: DateTime): IndexedSeq[CorporateAction] = {
    val history = findCorporateActionHistory(securityId)
    findCorporateActionsFromHistory(history, startTime, endTime)
  }
  def findCorporateActions(securityIds: IndexedSeq[Int], startTime: DateTime, endTime: DateTime): IndexedSeq[CorporateAction] =
    securityIds.flatMap(findCorporateActions(_, startTime, endTime))

  def findEodBarPriorToCorporateAction(corporateAction: CorporateAction): Option[Bar] =
    findEodBarPriorTo(corporateAction.exDate, corporateAction.securityId)

  // computes a cumulative price adjustment factor
  def cumulativePriceAdjustmentFactor(securityId: SecurityId, startTime: DateTime, endTime: DateTime): BigDecimal =
    priceAdjustmentFactors(securityId, startTime, endTime).map(_.adjustmentFactor).foldLeft(BigDecimal(1))(_ * _)

  /**
   * Returns a sequence of [corporate-action, prior-eod-bar, adjustment-factor] tuples ordered in ascending (i.e. oldest to most recent) order of the corporate action's ex-date.
   * The first element of the tuple, <corporate-action> is the corporate action from which the <adjustment-factor> is computed.
   * The second element of the tuple, <prior-eod-bar> is the most recent EOD bar prior to the <corporate-action>.
   * The last element of the tuple, <adjustment-factor> is the adjustment factor for the given <corporate-action>.
   * NOTE:
   *   The <adjustment-factor> can be
   *   multiplied by a particular unadjusted historical price in order to compute a corporate-action-adjusted historical price. A given unadjusted
   *   historical share count can be divided by the <adjustment-factor> to compute the associated corporate-action-adjusted historical share count (e.g.
   *   to produce an adjusted share volume or an adjusted "shares outstanding" measurement).
   *   Each adjustment-factor is not cumulative, it is specifically tied to a particular corporate-action.
   *   The definition of the adjustment-factor is taken from http://www.crsp.com/documentation/product/stkind/definitions/factor_to_adjust_price_in_period.html:
   *   "Factor from a base date used to adjust prices after distributions so that equivalent comparisons can be made between prices before and after the distribution."
   */
  def priceAdjustmentFactors(securityId: SecurityId, startTime: DateTime, endTime: DateTime): IndexedSeq[AdjustmentFactor] = {
    if (isBefore(startTime, endTime)) {
      val corporateActions = findCorporateActions(securityId, startTime, endTime)
      val corporateActionEodBarPairs = corporateActions.map(corporateAction => (corporateAction, findEodBarPriorToCorporateAction(corporateAction)) )
      corporateActionEodBarPairs.foldLeft(Vector[AdjustmentFactor]()) { (adjustmentFactors, actionBarPair) =>
        val (corporateAction, priorEodBar) = actionBarPair
        adjustmentFactors :+ AdjustmentFactor(corporateAction, priorEodBar, computeAdjustmentFactor(corporateAction, priorEodBar, adjustmentFactors))
      }
    } else Vector[AdjustmentFactor]()
  }

  def computeAdjustmentFactor(corporateAction: CorporateAction, priorEodBar: Option[Bar], priorAdjustmentFactors: IndexedSeq[AdjustmentFactor]): BigDecimal = {
    corporateAction match {
      case Split(_, _, ratio) => computeSplitAdjustmentFactor(ratio)
      case CashDividend(symbol, _, exDate, _, _, amount) => computeDividendAdjustmentFactor(corporateAction.asInstanceOf[CashDividend], priorEodBar, priorAdjustmentFactors)
    }
  }

  /**
   * See http://www.crsp.com/documentation/product/stkind/definitions/factor_to_adjust_price_in_period.html for implementation notes.
   * Returns an adjustment factor that:
   * 1. when multiplied by an unadjusted stock price, yields an adjusted stock price. i.e. unadjusted-price * adjustment-factor = adjusted-price
   * 2. when divided into an unadjusted share count, yields an adjusted share count. i.e. unadjusted-qty / adjustment-factor = adjusted-qty
   */
  def computeSplitAdjustmentFactor(splitRatio: BigDecimal): BigDecimal = 1 / splitRatio

  /**
   * See http://www.crsp.com/documentation/product/stkind/definitions/factor_to_adjust_price_in_period.html for implementation notes.
   * <prior-eod-bar> is the most recent EOD bar prior to the ex-date of <dividend>
   *
   * Returns an adjustment factor that:
   * 1. when multiplied by an unadjusted stock price, yields an adjusted stock price. i.e. unadjusted-price * adjustment-factor = adjusted-price
   * 2. when divided into an unadjusted share count, yields an adjusted share count. i.e. unadjusted-qty / adjustment-factor = adjusted-qty
   */
  def computeDividendAdjustmentFactor(dividend: CashDividend, priorEodBar: Option[Bar], priorAdjustmentFactors: IndexedSeq[AdjustmentFactor]): BigDecimal = {
    priorEodBar.map(eodBar =>
      1 - dividend.amount / (barClose(eodBar) * computeCumulativeDividendAdjustmentFactor(dividend, eodBar, priorAdjustmentFactors))
    ).getOrElse(1)
  }

  /**
   * priorAdjustmentFactors is a sequence of AdjustmentFactor(corporate-action, prior-eod-bar, adjustment-factor) tuples ordered in ascending
   *   (i.e. oldest to most recent) order of the corporate action's ex-date.
   */
  def computeCumulativeDividendAdjustmentFactor(dividend: CashDividend, priorEodBar: Bar, priorAdjustmentFactors: IndexedSeq[AdjustmentFactor]): BigDecimal = {
    val adjustmentFactorsInDescendingOrderOfExDate = priorAdjustmentFactors.reverse
    val applicableAdjustmentFactors = adjustmentFactorsInDescendingOrderOfExDate.takeWhile(_.priorEodBar.get == priorEodBar)
    applicableAdjustmentFactors.map(_.adjustmentFactor).foldLeft(BigDecimal(1))(_ * _)
  }

  /**
   * Given a price, <price>, of <symbol> that was observed at <price-observation-time>,
   * returns an adjusted price that (using <price> as a base price) has been adjusted for corporate actions that take effect between
   * <price-observation-time> and <adjustment-time>.
   * Assumes <price-observation-time> occurred strictly before <adjustment-time> (i.e. <price-observation-time> is strictly
   * older than <adjustment-time>).
   * This function can be interpreted as:
   * "Adjust the price, <price>, of <symbol>, that was observed at <price-observation-time> for corporate actions that took effect between
   * <price-observation-time> and <adjustment-time>. The adjusted price is the price that one would expect for <symbol> to trade at as of
   * <adjustment-time>"
   * NOTE:
   *   See http://www.investopedia.com/ask/answers/06/adjustedclosingprice.asp#axzz24Wa9LgDj for instructions on how to adjust a price for splits.
   *   See http://www.quantshare.com/sa-112-stock-split-dividend
   *   or
   *   http://help.yahoo.com/kb/index?locale=en_US&page=content&y=PROD_FIN&id=SLN2311&impressions=true
   *   http://help.yahoo.com/kb/index?locale=en_US&page=content&y=PROD_FIN&id=SLN2311&actp=lorax&pir=wMsp3EFibUlGxLjgTY.StSPNcMXGv318Io7yMwp2vXYNYOLFM2Y-
   *   for instructions on how to adjust a price for cash dividends.
   */
  def adjustPriceForCorporateActions(price: BigDecimal, securityId: SecurityId, priceObservationTime: DateTime, adjustmentTime: DateTime): BigDecimal =
    price * cumulativePriceAdjustmentFactor(securityId, priceObservationTime, adjustmentTime)

  def adjustPortfolioForCorporateActions[StateT <: State[StateT]](currentState: StateT, earlierObservationTime: DateTime, laterObservationTime: DateTime): StateT = {
    val portfolio = currentState.portfolio
    val securityIds = portfolio.stocks.keys.toVector
    val corporateActions = findCorporateActions(securityIds, earlierObservationTime, laterObservationTime)
//    println(s"********* Corporate Actions (for portfolio): $corporateActions for $symbols ; between $earlierObservationTime and $laterObservationTime")
    corporateActions.foldLeft(currentState)((updatedState, corporateAction) => adjustPortfolio(corporateAction, updatedState))
  }

  def adjustOpenOrdersForCorporateActions(openOrders: IndexedSeq[Order],
                                          earlierObservationTime: DateTime,
                                          laterObservationTime: DateTime): IndexedSeq[Order] = {
    if (openOrders.isEmpty) Vector[Order]()
    else {
      val securityIds = openOrders.map(_.securityId)
      val corporateActions = findCorporateActions(securityIds, earlierObservationTime, laterObservationTime)
      val corporateActionsPerSymbol = corporateActions.groupBy(_.securityId)
//      println(s"********* Corporate Actions (for open orders): $corporateActions for $symbols ; between $earlierObservationTime and $laterObservationTime")
      openOrders.map { (openOrder) =>
        val corporateActionsForSymbol = corporateActionsPerSymbol.getOrElse(openOrder.securityId, Vector[CorporateAction]())
        corporateActionsForSymbol.foldLeft(openOrder)((order, corporateAction) => adjustOpenOrder(corporateAction, order))
      }
    }
  }

  def adjustPortfolio[StateT <: State[StateT]](corporateAction: CorporateAction, currentState: StateT): StateT = corporateAction match {
    case split: Split => adjustPortfolio(split, currentState)
    case dividend: CashDividend => adjustPortfolio(dividend, currentState)
  }

  /**
   * Given a portfolio and split, this function applies the split to the portfolio and returns a split-adjusted portfolio.
   * Note:
   *   new holdings = old holdings * split ratio
   */
  def adjustPortfolio[StateT <: State[StateT]](split: Split, currentState: StateT): StateT = {
    val portfolio = currentState.portfolio
    val securityId = split.securityId
    val exDate = split.exDate
    val splitRatio = split.ratio
    val qty = sharesOnHand(portfolio, securityId)
    val adjQty = qty * splitRatio
    val adjSharesOnHand = floor(adjQty).toLong
    val fractionalShareQty = adjQty - adjSharesOnHand
    val eodBar = findEodBarPriorTo(midnight(exDate), securityId)
    eodBar.map { eodBar =>
      val closingPrice = barClose(eodBar)
      val splitAdjustedSharePrice = adjustPriceForCorporateActions(closingPrice, securityId, eodBar.endTime, exDate)
      val fractionalShareCashValue = fractionalShareQty * splitAdjustedSharePrice
      val adjustedPortfolio = threadThrough(portfolio)(setSharesOnHand(_, securityId, adjSharesOnHand),
                                                       addCash(_, fractionalShareCashValue))
      val updatedTransactionLog = currentState.transactions :+ SplitAdjustment(split.securityId,
                                                                               split.exDate,
                                                                               split.ratio,
                                                                               currentState.time,
                                                                               adjSharesOnHand - qty,
                                                                               fractionalShareCashValue)
      currentState.copy(portfolio = adjustedPortfolio, transactions = updatedTransactionLog)
    }.getOrElse(currentState)
  }

  def adjustPortfolio[StateT <: State[StateT]](dividend: CashDividend, currentState: StateT): StateT = {
    val portfolio = currentState.portfolio
    val qty = sharesOnHand(portfolio, dividend.securityId)
    val dividendPaymentAmount = computeDividendPaymentAmount(portfolio, dividend, qty)
    val adjustedPortfolio = addCash(portfolio, dividendPaymentAmount)
    val updatedTransactionLog = currentState.transactions :+ CashDividendPayment(dividend.securityId,
                                                                                 dividend.exDate,
                                                                                 dividend.payableDate,
                                                                                 dividend.amount,
                                                                                 currentState.time,
                                                                                 qty,
                                                                                 dividendPaymentAmount)
    currentState.copy(portfolio = adjustedPortfolio, transactions = updatedTransactionLog)
  }

  def adjustOpenOrder(corporateAction: CorporateAction, openOrder: Order): Order = corporateAction match {
    case split: Split => adjustOpenOrder(split, openOrder)
    case dividend: CashDividend => adjustOpenOrder(dividend, openOrder)
  }

  def adjustOpenOrder(split: Split, openOrder: Order): Order = {
    val splitRatio = split.ratio
    val orderQty = openOrder.qty
    val adjQty = floor(orderQty * splitRatio).toLong
    openOrder match {
      case limitOrder: LimitOrder =>
        val limitPrice = limitOrder.limitPrice
        val adjLimitPrice = limitPrice / splitRatio
        threadThrough(limitOrder)(setOrderQty(_, adjQty),
                                  setLimitPrice(_, adjLimitPrice))
      case marketOrder: MarketOrder => setOrderQty(marketOrder, adjQty)
    }
  }

  def adjustOpenOrder(dividend: CashDividend, openOrder: Order): Order = openOrder

  // returns the amount of cash the given portfolio is entitled to receive from the given cash-dividend
  def computeDividendPaymentAmount(portfolio: Portfolio, cashDividend: CashDividend, sharesOnHand: Long): BigDecimal = {
    sharesOnHand * cashDividend.amount
  }

  // returns a split adjusted share quantity, given an unadjusted share quantity
  def adjustShareQtyForCorporateActions(unadjustedQty: BigDecimal, securityId: SecurityId, earlierObservationTime: DateTime, laterObservationTime: DateTime): BigDecimal = {
    unadjustedQty / cumulativeShareQtyAdjustmentFactor(securityId, earlierObservationTime, laterObservationTime)
  }

  // computes a cumulative share quantity adjustment factor
  def cumulativeShareQtyAdjustmentFactor(securityId: SecurityId, earlierObservationTime: DateTime, laterObservationTime: DateTime): BigDecimal =
    shareQtyAdjustmentFactors(securityId, earlierObservationTime, laterObservationTime).map(_.adjustmentFactor).foldLeft(BigDecimal(1))(_ * _)

  /**
   * Returns a sequence of AdjustmentFactors ordered in ascending (i.e. oldest to most recent) order of the corporate action's ex-date.
   * The first element of the tuple, <corporate-action> is the corporate action from which the <adjustment-factor> is computed.
   * The last element of the tuple, <adjustment-factor> is the adjustment factor for the given <corporate-action>.
   * NOTE:
   *   A given unadjusted historical share count can be divided by the <adjustment-factor> to compute the associated
   *   corporate-action-adjusted historical share count (e.g. to produce an adjusted share volume or an adjusted "shares outstanding"
   *   measurement).
   *   Each adjustment-factor is not cumulative, it is specifically tied to a particular corporate-action.
   *   The definition of the adjustment-factor is taken from http://www.crsp.com/documentation/product/stkind/definitions/factor_to_adjust_price_in_period.html:
   *   "Factor from a base date used to adjust prices after distributions so that equivalent comparisons can be made between prices
   *   before and after the distribution."
   */
  def shareQtyAdjustmentFactors(securityId: SecurityId, earlierObservationTime: DateTime, laterObservationTime: DateTime): IndexedSeq[QtyAdjustmentFactor] = {
    if (isBefore(earlierObservationTime, laterObservationTime)) {
      val corporateActions = findCorporateActions(securityId, earlierObservationTime, laterObservationTime)
      corporateActions.foldLeft(Vector[QtyAdjustmentFactor]()) { (qtyAdjustmentFactors, corporateAction) =>
        qtyAdjustmentFactors :+ QtyAdjustmentFactor(corporateAction, computeShareQtyAdjustmentFactor(corporateAction))
      }
    } else Vector[QtyAdjustmentFactor]()
  }

  def computeShareQtyAdjustmentFactor(corporateAction: CorporateAction): BigDecimal = {
    corporateAction match {
      case Split(_, _, ratio) => computeSplitAdjustmentFactor(ratio)
      case _ => BigDecimal(1)
    }
  }

}