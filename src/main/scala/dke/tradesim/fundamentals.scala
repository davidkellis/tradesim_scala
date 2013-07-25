package dke.tradesim

import datetimeUtils._
import quarterlyReports._
import quotes._
import adjustedQuotes._
import splitsDividends._
import org.joda.time.DateTime
import dke.tradesim.core.{QuarterlyReport, NumericAttribute, StatementType}

object fundamentals {
  object BalanceSheetAttributes {
    val NetIncome = "total net income"
    val SharesOutstanding = "total common shares out"
    val BasicWeightedShares = "basic weighted shares"
  }

  object IncomeStatementAttributes {
    val PreferredDividends = "preferred dividends"
  }

  def sharesOutstanding(quarterlyReport: QuarterlyReport): Option[BigDecimal] =
    numericQuarterlyReportAttribute(quarterlyReport, StatementType.BalanceSheet, BalanceSheetAttributes.SharesOutstanding)

  def sharesOutstanding(time: DateTime, symbol: String): Option[BigDecimal] =
    numericQuarterlyReportAttribute(time, symbol, StatementType.BalanceSheet, BalanceSheetAttributes.SharesOutstanding)

  def basicWeightedSharesOutstanding(quarterlyReport: QuarterlyReport): Option[BigDecimal] =
    numericQuarterlyReportAttribute(quarterlyReport, StatementType.BalanceSheet, BalanceSheetAttributes.BasicWeightedShares)

  def basicWeightedSharesOutstanding(time: DateTime, symbol: String): Option[BigDecimal] =
    numericQuarterlyReportAttribute(time, symbol, StatementType.BalanceSheet, BalanceSheetAttributes.BasicWeightedShares)

//  def sharesOutstanding(time: DateTime, quarterlyReport: QuarterlyReport): Option[BigDecimal] = {
//    val sharesOutAttribute = quarterlyReport.balanceSheet.get(BalanceSheetAttributes.SharesOutstanding)
//    val qty = sharesOutAttribute match {
//      case Some(NumericAttribute(shareCount)) => Option(shareCount)
//      case _ => None
//    }
//    qty.map(qty => adjustShareQtyForCorporateActions(qty, quarterlyReport.symbol, quarterlyReport.publicationTime, time))
//  }
//
//  def sharesOutstanding(time: DateTime, symbol: String): Option[BigDecimal] = {
//    val quarterlyReport = findQuarterlyReport(time, symbol)
//    quarterlyReport.flatMap { quarterlyReport =>
//      val sharesOutAttribute = quarterlyReport.balanceSheet.get(BalanceSheetAttributes.SharesOutstanding)
//      val qty = sharesOutAttribute match {
//        case Some(NumericAttribute(shareCount)) => Option(shareCount)
//        case _ => None
//      }
//      qty.map(qty => adjustShareQtyForCorporateActions(qty, symbol, quarterlyReport.publicationTime, time))
//    }
//  }

  def netIncome(quarterlyReport: QuarterlyReport): Option[BigDecimal] =
    numericQuarterlyReportAttribute(quarterlyReport, StatementType.BalanceSheet, BalanceSheetAttributes.NetIncome)

  def netIncome(time: DateTime, symbol: String): Option[BigDecimal] =
    numericQuarterlyReportAttribute(time, symbol, StatementType.BalanceSheet, BalanceSheetAttributes.NetIncome)

  // See http://www.investopedia.com/terms/e/eps.asp and http://www.investopedia.com/terms/b/basic-earnings-per-share.asp
  // Basic EPS = (Net Income - Dividends on Preferred Stock) / Average Outstanding Shares
  def basicEarningsPerShare(quarterlyReport: QuarterlyReport): Option[BigDecimal] = {
    for (
      netIncome <- netIncome(quarterlyReport);
      preferredDividends <- numericQuarterlyReportAttribute(quarterlyReport, StatementType.IncomeStatement, IncomeStatementAttributes.PreferredDividends).orElse(Some(BigDecimal(0)));
      averageSharesOutstanding <- basicWeightedSharesOutstanding(quarterlyReport)
    ) yield (netIncome - preferredDividends) / averageSharesOutstanding
  }

  def basicEarningsPerShare(time: DateTime, symbol: String): Option[BigDecimal] = {
    for (
      netIncome <- netIncome(time, symbol);
      preferredDividends <- numericQuarterlyReportAttribute(time, symbol, StatementType.IncomeStatement, IncomeStatementAttributes.PreferredDividends);
      averageSharesOutstanding <- basicWeightedSharesOutstanding(time, symbol)
    ) yield (netIncome - preferredDividends) / averageSharesOutstanding
  }

  // see http://www.investopedia.com/terms/p/price-earningsratio.asp
  def priceToEarnings(time: DateTime, symbol: String): Option[BigDecimal] = {
    for {
      price <- adjEodClose(time, symbol)
      lastFourQuarterlyReports <- findQuarterlyReports(time, symbol, 4)
      lastFourQuarterlyEpsNumbers = lastFourQuarterlyReports.map(report => basicEarningsPerShare(report))
      if lastFourQuarterlyEpsNumbers.length == 4
      eps = lastFourQuarterlyEpsNumbers.flatten.reduceLeft(_ + _)
    } yield price / eps
  }
}
