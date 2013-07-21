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
    val SharesOutstanding = "total common shares out"
  }

  def unadjustedSharesOutstanding(time: DateTime, symbol: String): Option[BigDecimal] = {
    val sharesOut = quarterlyReportAttribute(time, symbol, StatementType.BalanceSheet, BalanceSheetAttributes.SharesOutstanding)
    sharesOut match {
      case Some(NumericAttribute(shareCount)) => Option(shareCount)
      case _ => None
    }
  }

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


//  def earningsPerShare(time: DateTime, symbol: String): Option[BigDecimal] = {
//    val quarterlyReport = findQuarterlyReport(time, symbol)
//  }
//
//  def priceToEarnings(time: DateTime, symbol: String): Option[BigDecimal] = {
//    val quarterlyReport = findQuarterlyReport(time, symbol)
//    val price = adjEodSimQuote(time, symbol)
//    quarterlyReport.map(report => report.)
//  }
}
