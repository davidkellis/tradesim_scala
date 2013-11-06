package dke.tradesim

import datetimeUtils._
import quarterlyReports._
import quotes._
import adjustedQuotes._
import splitsDividends._
import org.joda.time.DateTime
import dke.tradesim.core.{SecurityId, QuarterlyReport, NumericAttribute, StatementType}

object fundamentals {
  object BalanceSheetAttributes {
    val SharesOutstanding = "total common shares out"
    val BasicWeightedShares = "basic weighted shares"
    val DilutedWeightedShares = "diluted weighted shares"
  }

  object IncomeStatementAttributes {
    val NetIncome = "total net income"
    val PreferredDividends = "preferred dividends"
  }

  def sharesOutstanding(quarterlyReport: QuarterlyReport): Option[BigDecimal] =
    numericQuarterlyReportAttribute(quarterlyReport, StatementType.BalanceSheet, BalanceSheetAttributes.SharesOutstanding)

  def sharesOutstanding(time: DateTime, securityId: SecurityId): Option[BigDecimal] =
    numericQuarterlyReportAttribute(time, securityId, StatementType.BalanceSheet, BalanceSheetAttributes.SharesOutstanding)

  def basicWeightedSharesOutstanding(quarterlyReport: QuarterlyReport): Option[BigDecimal] =
    numericQuarterlyReportAttribute(quarterlyReport, StatementType.BalanceSheet, BalanceSheetAttributes.BasicWeightedShares)

  def basicWeightedSharesOutstanding(time: DateTime, securityId: SecurityId): Option[BigDecimal] =
    numericQuarterlyReportAttribute(time, securityId, StatementType.BalanceSheet, BalanceSheetAttributes.BasicWeightedShares)

  def dilutedWeightedSharesOutstanding(quarterlyReport: QuarterlyReport): Option[BigDecimal] =
    numericQuarterlyReportAttribute(quarterlyReport, StatementType.BalanceSheet, BalanceSheetAttributes.DilutedWeightedShares)

  def dilutedWeightedSharesOutstanding(time: DateTime, securityId: SecurityId): Option[BigDecimal] =
    numericQuarterlyReportAttribute(time, securityId, StatementType.BalanceSheet, BalanceSheetAttributes.DilutedWeightedShares)

//  def sharesOutstanding(time: DateTime, quarterlyReport: QuarterlyReport): Option[BigDecimal] = {
//    val sharesOutAttribute = quarterlyReport.balanceSheet.get(BalanceSheetAttributes.SharesOutstanding)
//    val qty = sharesOutAttribute match {
//      case Some(NumericAttribute(shareCount)) => Option(shareCount)
//      case _ => None
//    }
//    qty.map(qty => adjustShareQtyForCorporateActions(qty, quarterlyReport.symbol, quarterlyReport.publicationTime, time))
//  }
//
//  def sharesOutstanding(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
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
    numericQuarterlyReportAttribute(quarterlyReport, StatementType.IncomeStatement, IncomeStatementAttributes.NetIncome)

  def netIncome(time: DateTime, securityId: SecurityId): Option[BigDecimal] =
    numericQuarterlyReportAttribute(time, securityId, StatementType.IncomeStatement, IncomeStatementAttributes.NetIncome)

  // See http://www.investopedia.com/terms/e/eps.asp and http://www.investopedia.com/terms/b/basic-earnings-per-share.asp
  // Basic EPS = (Net Income - Dividends on Preferred Stock) / Average Outstanding Shares
  def basicEarningsPerShareMRQ(quarterlyReport: QuarterlyReport): Option[BigDecimal] = {
    for {
      netIncome <- netIncome(quarterlyReport)
      preferredDividends <- numericQuarterlyReportAttribute(quarterlyReport, StatementType.IncomeStatement, IncomeStatementAttributes.PreferredDividends).orElse(Some(BigDecimal(0)))
      averageSharesOutstanding <- basicWeightedSharesOutstanding(quarterlyReport)
    } yield (netIncome - preferredDividends) / averageSharesOutstanding
  }

  def basicEarningsPerShareMRQ(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
    for {
      netIncome <- netIncome(time, securityId)
      preferredDividends <- numericQuarterlyReportAttribute(time, securityId, StatementType.IncomeStatement, IncomeStatementAttributes.PreferredDividends)
      averageSharesOutstanding <- basicWeightedSharesOutstanding(time, securityId)
    } yield (netIncome - preferredDividends) / averageSharesOutstanding
  }

  def dilutedEarningsPerShareMRQ(quarterlyReport: QuarterlyReport): Option[BigDecimal] = {
    for {
      netIncome <- netIncome(quarterlyReport)
      preferredDividends <- numericQuarterlyReportAttribute(quarterlyReport, StatementType.IncomeStatement, IncomeStatementAttributes.PreferredDividends).orElse(Some(BigDecimal(0)))
      averageSharesOutstanding <- dilutedWeightedSharesOutstanding(quarterlyReport)
    } yield (netIncome - preferredDividends) / averageSharesOutstanding
  }

  def dilutedEarningsPerShareMRQ(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
    for {
      netIncome <- netIncome(time, securityId)
      preferredDividends <- numericQuarterlyReportAttribute(time, securityId, StatementType.IncomeStatement, IncomeStatementAttributes.PreferredDividends)
      averageSharesOutstanding <- dilutedWeightedSharesOutstanding(time, securityId)
    } yield (netIncome - preferredDividends) / averageSharesOutstanding
  }

  def basicEarningsPerShareTTM(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
    for {
      lastFourQuarterlyReports <- findQuarterlyReports(time, securityId, 4)
      lastFourQuarterlyNetIncomeNumbers = lastFourQuarterlyReports.map(report => netIncome(report))
      if lastFourQuarterlyNetIncomeNumbers.length == 4
      netIncome = lastFourQuarterlyNetIncomeNumbers.flatten.reduceLeft(_ + _)
      preferredDividends <- numericQuarterlyReportAttribute(time, securityId, StatementType.IncomeStatement, IncomeStatementAttributes.PreferredDividends)
      averageSharesOutstanding <- basicWeightedSharesOutstanding(time, securityId)
    } yield (netIncome - preferredDividends) / averageSharesOutstanding
  }

  def dilutedEarningsPerShareTTM(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
    for {
      lastFourQuarterlyReports <- findQuarterlyReports(time, securityId, 4)
      lastFourQuarterlyNetIncomeNumbers = lastFourQuarterlyReports.map(report => netIncome(report))
      if lastFourQuarterlyNetIncomeNumbers.length == 4
      netIncome = lastFourQuarterlyNetIncomeNumbers.flatten.reduceLeft(_ + _)
      preferredDividends <- numericQuarterlyReportAttribute(time, securityId, StatementType.IncomeStatement, IncomeStatementAttributes.PreferredDividends)
      averageSharesOutstanding <- dilutedWeightedSharesOutstanding(time, securityId)
    } yield (netIncome - preferredDividends) / averageSharesOutstanding
  }

  // see http://www.investopedia.com/terms/p/price-earningsratio.asp
  def basicPriceToEarnings(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
    for {
      price <- adjEodClose(time, securityId)
      lastFourQuarterlyReports <- findQuarterlyReports(time, securityId, 4)
      lastFourQuarterlyEpsNumbers = lastFourQuarterlyReports.map(report => basicEarningsPerShareMRQ(report))
      if lastFourQuarterlyEpsNumbers.length == 4
      eps = lastFourQuarterlyEpsNumbers.flatten.reduceLeft(_ + _)
    } yield price / eps
  }

  def dilutedPriceToEarnings(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
    for {
      price <- adjEodClose(time, securityId)
      lastFourQuarterlyReports <- findQuarterlyReports(time, securityId, 4)
      lastFourQuarterlyEpsNumbers = lastFourQuarterlyReports.map(report => dilutedEarningsPerShareMRQ(report))
      if lastFourQuarterlyEpsNumbers.length == 4
      eps = lastFourQuarterlyEpsNumbers.flatten.reduceLeft(_ + _)
    } yield price / eps
  }

  def basicMarketCapitalization(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
    for {
      price <- adjEodClose(time, securityId)
      averageSharesOutstanding <- basicWeightedSharesOutstanding(time, securityId)
    } yield price * averageSharesOutstanding
  }

  def dilutedMarketCapitalization(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
    for {
      price <- adjEodClose(time, securityId)
      averageSharesOutstanding <- dilutedWeightedSharesOutstanding(time, securityId)
    } yield price * averageSharesOutstanding
  }
}

// todo:
// 1. select subset of companies that meet some criteria at time t.
// 2. rank companies by some set of attributes
// 3. build company snapshots containing a given set of attributes along side a set of future return yields for each of several different time frames s.t. each timeframe begins at the time the snapshot is taken (or immediately following?)