package dke.tradesim

import datetimeUtils._
import quarterlyReports._
import quotes._
import adjustedQuotes._
import splitsDividends._
import org.joda.time.DateTime
import dke.tradesim.core.{NumericAttribute, StatementType}

object fundamentals {
  object BalanceSheetAttributes {
    val NetIncome = "total net income"
    val SharesOutstanding = "total common shares out"
  }

  def unadjustedSharesOutstanding(time: DateTime, symbol: String): Option[BigDecimal] =
    numericQuarterlyReportAttribute(time, symbol, StatementType.BalanceSheet, BalanceSheetAttributes.SharesOutstanding)

  def sharesOutstanding(time: DateTime, symbol: String): Option[BigDecimal] = {
    val quarterlyReport = findQuarterlyReport(time, symbol)
    quarterlyReport.flatMap { quarterlyReport =>
      val sharesOutAttribute = quarterlyReport.balanceSheet.get(BalanceSheetAttributes.SharesOutstanding)
      val qty = sharesOutAttribute match {
        case Some(NumericAttribute(shareCount)) => Option(shareCount)
        case _ => None
      }
      qty.map(qty => adjustShareQtyForCorporateActions(qty, symbol, quarterlyReport.publicationTime, time))
    }
  }

  def netIncome(time: DateTime, symbol: String): Option[BigDecimal] =
    numericQuarterlyReportAttribute(time, symbol, StatementType.BalanceSheet, BalanceSheetAttributes.NetIncome)

  // See http://www.investopedia.com/terms/e/eps.asp
  // EPS = (Net Income - Dividends on Preferred Stock) / Average Outstanding Shares
  // This implementation is: EPS = Net Income / Outstanding Shares
  def earningsPerShare(time: DateTime, symbol: String): Option[BigDecimal] = {
    netIncome(time, symbol).flatMap { netIncome =>
      sharesOutstanding(time, symbol).map(sharesOutstanding => netIncome / sharesOutstanding)
    }
  }
//
//  def priceToEarnings(time: DateTime, symbol: String): Option[BigDecimal] = {
//    val quarterlyReport = findQuarterlyReport(time, symbol)
//    val price = adjEodSimQuote(time, symbol)
//    quarterlyReport.map(report => report.)
//  }
}
