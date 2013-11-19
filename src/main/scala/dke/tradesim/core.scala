package dke.tradesim

import org.joda.time.{DateTime}
import dke.tradesim.datetimeUtils.Datestamp

object core {
  type SecurityId = Int
  case class SecurityIds(securityIds: Seq[SecurityId])

  type StockHoldings = Map[SecurityId, Long]

  case class Portfolio(cash: BigDecimal,
                       stocks: StockHoldings)

  sealed trait Transaction

  type TransactionLog = IndexedSeq[Transaction]
  case class Transactions(transactions: Seq[Transaction])

  sealed trait Order extends Transaction {
    val time: DateTime
    val securityId: SecurityId
    val qty: Long
    val fillPrice: Option[BigDecimal]
    def changeQty(newQty: Long): Order
    def changeFillPrice(newFillPrice: BigDecimal): Order
  }

  type MarketOrder = Order
  sealed trait LimitOrder extends Order {
    val limitPrice: BigDecimal
    def changeLimitPrice(newLimitPrice: BigDecimal): LimitOrder
  }

  sealed trait BuyOrder extends Order
  sealed trait SellOrder extends Order

  case class MarketBuy(time: DateTime, securityId: SecurityId, qty: Long, fillPrice: Option[BigDecimal] = None) extends MarketOrder with BuyOrder {
    def changeQty(newQty: Long): MarketBuy = this.copy(qty = newQty)
    def changeFillPrice(newFillPrice: BigDecimal): MarketBuy = this.copy(fillPrice = Option(newFillPrice))
  }

  case class MarketSell(time: DateTime, securityId: SecurityId, qty: Long, fillPrice: Option[BigDecimal] = None) extends MarketOrder with SellOrder {
    def changeQty(newQty: Long): MarketSell = this.copy(qty = newQty)
    def changeFillPrice(newFillPrice: BigDecimal): MarketSell = this.copy(fillPrice = Option(newFillPrice))
  }

  case class LimitBuy(time: DateTime, securityId: SecurityId, qty: Long, limitPrice: BigDecimal, fillPrice: Option[BigDecimal] = None) extends LimitOrder with BuyOrder {
    def changeQty(newQty: Long): LimitBuy = this.copy(qty = newQty)
    def changeLimitPrice(newLimitPrice: BigDecimal): LimitBuy = this.copy(limitPrice = newLimitPrice)
    def changeFillPrice(newFillPrice: BigDecimal): LimitBuy = this.copy(fillPrice = Option(newFillPrice))
  }

  case class LimitSell(time: DateTime, securityId: SecurityId, qty: Long, limitPrice: BigDecimal, fillPrice: Option[BigDecimal] = None) extends LimitOrder with SellOrder {
    def changeQty(newQty: Long): LimitSell = this.copy(qty = newQty)
    def changeLimitPrice(newLimitPrice: BigDecimal): LimitSell = this.copy(limitPrice = newLimitPrice)
    def changeFillPrice(newFillPrice: BigDecimal): LimitSell = this.copy(fillPrice = Option(newFillPrice))
  }

  type PriceBarFn = (DateTime, SecurityId) => Option[Bar]
  type PriceQuoteFn = (DateTime, SecurityId) => Option[BigDecimal]

  case class PortfolioValue(time: DateTime, value: BigDecimal)
  case class PortfolioValues(portfolioValues: Seq[PortfolioValue])

  trait State[SubType] {
    val previousTime: DateTime
    val time: DateTime
    val portfolio: Portfolio
    val orders: IndexedSeq[Order]
    val transactions: TransactionLog
    val portfolioValueHistory: Seq[PortfolioValue]

    def copy(previousTime: DateTime = previousTime,
             time: DateTime = time,
             portfolio: Portfolio = portfolio,
             orders: IndexedSeq[Order] = orders,
             transactions: TransactionLog = transactions,
             portfolioValueHistory: Seq[PortfolioValue] = portfolioValueHistory): SubType

    def initializeState(time: DateTime, principal: BigDecimal): SubType = copy(time,
                                                                               time,
                                                                               Portfolio(principal, Map[SecurityId, Long]()),
                                                                               Vector(),
                                                                               Vector(),
                                                                               List())
  }

  case class Trial(securityIds: IndexedSeq[SecurityId],
                   principal: BigDecimal,
                   commissionPerTrade: BigDecimal,
                   commissionPerShare: BigDecimal,
                   startTime: DateTime,
                   endTime: DateTime,
                   incrementTime: (DateTime) => DateTime,
                   purchaseFillPrice: PriceQuoteFn,
                   saleFillPrice: PriceQuoteFn)

  case class Strategy[StateT <: State[StateT]](name: String,
                                               buildInitState: (Strategy[StateT], Trial) => StateT,
                                               buildNextState: (Strategy[StateT], Trial, StateT) => StateT,
                                               isFinalState: (Strategy[StateT], Trial, StateT) => Boolean)


  case class Industry(name: String)
  case class Sector(name: String)

  case class Exchange(id: Option[Int],
                      label: String,
                      name: Option[String])

  case class Security(id: Option[Int],
                      bbGid: String,
                      bbGcid: String,
                      kind: String,
                      exchangeId: Int,
                      symbol: String,
                      name: String,
                      startDate: Option[Datestamp],
                      endDate: Option[Datestamp],
                      cik: Option[Int],
                      active: Option[Boolean],
                      fiscalYearEndDate: Option[Int],
                      industryId: Option[Int],
                      sectorId: Option[Int])

  abstract class Bar {
    val securityId: SecurityId
    val startTime: DateTime
    val endTime: DateTime
    val open: BigDecimal
    val high: BigDecimal
    val low: BigDecimal
    val close: BigDecimal
    val volume: Long
  }

  case class EodBar(id: Option[Int],
                    securityId: SecurityId,
                    startTime: DateTime,
                    endTime: DateTime,
                    open: BigDecimal,
                    high: BigDecimal,
                    low: BigDecimal,
                    close: BigDecimal,
                    volume: Long) extends Bar


  trait CorporateAction {
    val securityId: SecurityId
    val exDate: DateTime
  }
  case class Split(securityId: SecurityId,
                   exDate: DateTime,
                   ratio: BigDecimal) extends CorporateAction

  case class SplitAdjustment(securityId: SecurityId,
                             exDate: DateTime,
                             ratio: BigDecimal,
                             adjustmentTime: DateTime,
                             shareQtyDelta: Long,
                             cashPayout: BigDecimal) extends Transaction

  // See http://www.investopedia.com/articles/02/110802.asp#axzz24Wa9LgDj for the various dates associated with dividend payments
  // See also http://www.sec.gov/answers/dividen.htm
  case class CashDividend(securityId: SecurityId,
                          declarationDate: Option[DateTime],    // date at which the announcement to shareholders/market that company will pay a dividend is made
                          exDate: DateTime,                     // on or after this date, the security trades without the dividend
                          recordDate: Option[DateTime],         // date at which shareholders of record are identified as recipients of the dividend
                          payableDate: Option[DateTime],        // date at which company issues payment of dividend
                          amount: BigDecimal) extends CorporateAction

  case class CashDividendPayment(securityId: SecurityId,
                                 exDate: DateTime,                     // on or after this date, the security trades without the dividend
                                 payableDate: Option[DateTime],        // date at which company issues payment of dividend
                                 amountPerShare: BigDecimal,           // amount of the dividend, per share
                                 adjustmentTime: DateTime,             // time at which the adjustment took place
                                 shareQty: Long,                       // number of shares on hand of <securityId>
                                 total: BigDecimal) extends Transaction

  trait StatementAttribute
  case class HeaderAttribute(text: String) extends StatementAttribute
  case class StringAttribute(value: String) extends StatementAttribute
  case class NumericAttribute(value: BigDecimal) extends StatementAttribute
//  case class RatioLineItem(attribute: String, value: String) extends LineItem
  type Statement = Map[String, StatementAttribute]
  type IncomeStatement = Statement
  type BalanceSheet = Statement
  type CashFlowStatement = Statement

  object StatementType extends Enumeration {
    val IncomeStatement, BalanceSheet, CashFlowStatement = Value
  }

  trait FinancialReport
  case class QuarterlyReport(securityId: SecurityId,
                             startTime: DateTime,
                             endTime: DateTime,
                             publicationTime: DateTime,
                             incomeStatement: IncomeStatement,
                             balanceSheet: BalanceSheet,
                             cashFlowStatement: CashFlowStatement) extends FinancialReport
  case class AnnualReport(securityId: SecurityId,
                          startTime: DateTime,
                          endTime: DateTime,
                          publicationTime: DateTime,
                          incomeStatement: IncomeStatement,
                          balanceSheet: BalanceSheet,
                          cashFlowStatement: CashFlowStatement) extends FinancialReport

  def extractNumericAttributes(statement: Statement): Map[String, BigDecimal] = {
    statement.flatMap {
      case (key, NumericAttribute(number)) => Some(key -> number)
      case _ => None
    }
  }

  // like the -> (thread) operator in clojure
  def threadThrough[T](o: T)(fns: Function[T, T]*): T = fns.foldLeft(o)((intermediateObject, transform) => transform(intermediateObject))
}
