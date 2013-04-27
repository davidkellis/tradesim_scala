package dke.tradesim

//import scala.collection.JavaConversions._
import org.joda.time.DateTime
import net.sf.ehcache.{Element, CacheManager}
import java.util.{NavigableMap, TreeMap}
import dke.tradesim.datetime._
import dke.tradesim.core.Bar
import dke.tradesim.quotes.{findEodBarPriorTo, barClose}

object splits_dividends {
  trait CorporateAction {
    val symbol: String
    val exDate: DateTime
  }
  case class Split(symbol: String,
                   exDate: DateTime,
                   ratio: BigDecimal) extends CorporateAction
  case class Dividend(symbol: String,
                      declarationDate: DateTime,
                      exDate: DateTime,
                      recordDate: DateTime,
                      payableDate: DateTime,
                      amount: BigDecimal) extends CorporateAction

  case class AdjustmentFactor(corporateAction: CorporateAction, priorEodBar: Option[Bar], adjustmentFactor: BigDecimal)

  def convertDbSplitRatio(dbSplitRatio: String): BigDecimal = {
    val numerator :: denominator :: _ = dbSplitRatio.split('/').map(BigDecimal(_)).toList
    numerator / denominator
  }

  def convertSplitDbRecord = ???
  def convertDividendDbRecord = ???
  def createCorporateAction = ???

  def queryCorporateActions(symbol: String): IndexedSeq[CorporateAction] = queryCorporateActions(Vector(symbol))
  def queryCorporateActions(symbols: IndexedSeq[String]): IndexedSeq[CorporateAction] = {
  }
  def queryCorporateActions(symbol: String, startTime: DateTime, endTime: DateTime): IndexedSeq[CorporateAction] = queryCorporateActions(Vector(symbol), startTime, endTime)
  def queryCorporateActions(symbols: IndexedSeq[String], startTime: DateTime, endTime: DateTime): IndexedSeq[CorporateAction] = {
  }

  type Timestamp = String
  type CorporateActionHistory = NavigableMap[Timestamp, CorporateAction]

  def loadCorporateActionHistory(symbol: String): CorporateActionHistory = {
    val corporateActions = queryCorporateActions(symbol)
    val corporateActionHistory = new TreeMap[Timestamp, CorporateAction]()
    for {corporateAction <- corporateActions} corporateActionHistory.put(timestamp(corporateAction.exDate), corporateAction)
    corporateActionHistory
  }

  val corporateActionCache = CacheManager.getInstance().getCache("corporateActionCache")

  def findCorporateActionHistory(symbol: String): CorporateActionHistory = {
    val corporateActionHistory = Option(corporateActionCache.get(symbol))
    corporateActionHistory match {
      case Some(corporateActionHistoryElement) => corporateActionHistoryElement.getObjectValue.asInstanceOf[CorporateActionHistory]
      case None =>
        val newCorporateActionHistory = loadCorporateActionHistory(symbol)
        corporateActionCache.put(new Element(symbol, newCorporateActionHistory))
        newCorporateActionHistory
    }
  }

  def findCorporateActionsFromHistory(history: CorporateActionHistory, startTime: DateTime, endTime: DateTime): IndexedSeq[CorporateAction] = {
    import scala.collection.JavaConversions.asScalaIterable
    val startTimestamp = timestamp(startTime)
    val endTimestamp = timestamp(endTime)
    val subHistory = history.subMap(startTimestamp, true, endTimestamp, true)
    val corporateActions = subHistory.values()
    corporateActions.toVector   // Iterable#toVector by implicit conversion
  }

  def findCorporateActions(symbol: String, startTime: DateTime, endTime: DateTime): IndexedSeq[CorporateAction] = {
    val history = findCorporateActionHistory(symbol)
    findCorporateActionsFromHistory(history, startTime, endTime)
  }
  def findCorporateActions(symbols: IndexedSeq[String], startTime: DateTime, endTime: DateTime): IndexedSeq[CorporateAction] =
    symbols.flatMap(findCorporateActions(_, startTime, endTime))

  def findEodBarPriorToCorporateAction(corporateAction: CorporateAction): Option[Bar] =
    findEodBarPriorTo(corporateAction.exDate, corporateAction.symbol)

  // computes a cumulative adjustment factor
  def cumulativePriceAdjustmentFactor(symbol: String, startTime: DateTime, endTime: DateTime): BigDecimal =
    priceAdjustmentFactors(symbol, startTime, endTime).map(_.adjustmentFactor).foldLeft(BigDecimal(1))(_ * _)

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
  def priceAdjustmentFactors(symbol: String, startTime: DateTime, endTime: DateTime): IndexedSeq[AdjustmentFactor] = {
    if (isBefore(startTime, endTime)) {
      val corporateActions = findCorporateActions(symbol, startTime, endTime)
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
      case Dividend(symbol, _, exDate, _, _, amount) => computeDividendAdjustmentFactor(corporateAction.asInstanceOf[Dividend], priorEodBar, priorAdjustmentFactors)
    }
  }

  def computeSplitAdjustmentFactor(splitRatio: BigDecimal): BigDecimal = 1 / splitRatio

  /*
   * See http://www.crsp.com/documentation/product/stkind/definitions/factor_to_adjust_price_in_period.html for implementation notes.
   * <prior-eod-bar> is the most recent EOD bar prior to the ex-date of <dividend>
   *
   * Returns an adjustment factor that:
   * 1. when multiplied by an unadjusted stock price, yields an adjusted stock price. i.e. unadjusted-price * adjustment-factor = adjusted-price
   * 2. when divided into an unadjusted share count, yields an adjusted share count. i.e. unadjusted-qty / adjustment-factor = adjusted-qty
   */
  def computeDividendAdjustmentFactor(dividend: Dividend, priorEodBar: Option[Bar], priorAdjustmentFactors: IndexedSeq[AdjustmentFactor]): BigDecimal = {
    priorEodBar.map(eodBar =>
      1 - dividend.amount / (barClose(eodBar) * computeCumulativeDividendAdjustmentFactor(dividend, eodBar, priorAdjustmentFactors))
    ).getOrElse(1)
  }

  /**
   * priorAdjustmentFactors is a sequence of AdjustmentFactor(corporate-action, prior-eod-bar, adjustment-factor) tuples ordered in ascending
   *   (i.e. oldest to most recent) order of the corporate action's ex-date.
   */
  def computeCumulativeDividendAdjustmentFactor(dividend: Dividend, priorEodBar: Bar, priorAdjustmentFactors: IndexedSeq[AdjustmentFactor]): BigDecimal = {
    val adjustmentFactorsInDescendingOrderOfExDate = priorAdjustmentFactors.reverse
    val applicableAdjustmentFactors = adjustmentFactorsInDescendingOrderOfExDate.takeWhile(_.priorEodBar == priorEodBar)
    applicableAdjustmentFactors.map(_.adjustmentFactor).foldLeft(BigDecimal(1))(_ * _)
  }

  /**
   * Given a price, <price>, of <symbol> that was observed at <price-observation-time>,
   * returns an adjusted price that (using <price> as a base price) has been adjusted for corporate actions that take effect between <price-observation-time> and <adjustment-time>.
   * Assumes <price-observation-time> occurred strictly before <adjustment-time> (i.e. <price-observation-time> is strictly older than <adjustment-time>).
   * This function can be interpreted as:
   * "Adjust the price, <price>, of <symbol>, that was observed at <price-observation-time> for corporate actions that took effect between
   * <price-observation-time> and <adjustment-time>. The adjusted price is the price that one would expect for <symbol> to trade at as of <adjustment-time>"
   * NOTE:
   *   See http://www.investopedia.com/ask/answers/06/adjustedclosingprice.asp#axzz24Wa9LgDj for instructions on how to adjust a price for splits.
   *   See http://www.quantshare.com/sa-112-stock-split-dividend
   *   or
   *   http://help.yahoo.com/kb/index?locale=en_US&page=content&y=PROD_FIN&id=SLN2311&impressions=true
   *   for instructions on how to adjust a price for cash dividends.
   */
  def adjustPriceForCorporateActions(price: BigDecimal, symbol: String, priceObservationTime: DateTime, adjustmentTime: DateTime): BigDecimal =
    price * cumulativePriceAdjustmentFactor(symbol, priceObservationTime, adjustmentTime)
}