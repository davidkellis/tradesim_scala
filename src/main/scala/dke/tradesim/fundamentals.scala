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
    val BasicWeightedShares = "basic weighted shares"
    val CashAndCashEquivalents = "cash & equivalents"
    val CurrentAssets = "total current assets"
    val CurrentLiabilities = "total current liabilities"
    val DilutedWeightedShares = "diluted weighted shares"
    val LongTermDebt = "long-term debt"
    val NetFixedAssets = "net fixed assets"
    val SharesOutstanding = "total common shares out"
    val ShortTermDebt = "short-term debt"
  }

  object IncomeStatementAttributes {
    val EBIT = "ebit"
    val NetIncome = "total net income"
    val PreferredDividends = "preferred dividends"
    val MinorityInterest = "minority interest"
  }

  object mrq {
    
    // See http://www.investopedia.com/terms/e/eps.asp and http://www.investopedia.com/terms/b/basic-earnings-per-share.asp
    // Basic EPS = (Net Income - Dividends on Preferred Stock) / Average Outstanding Shares
    def basicEarningsPerShare(quarterlyReport: QuarterlyReport): Option[BigDecimal] = {
      for {
        netIncome <- netIncome(quarterlyReport)
        preferredDividends <- preferredDividends(quarterlyReport).orElse(Some(BigDecimal(0)))
        averageSharesOutstanding <- basicWeightedSharesOutstanding(quarterlyReport)
      } yield (netIncome - preferredDividends) / averageSharesOutstanding
    }

    def basicEarningsPerShare(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
      for {
        netIncome <- netIncome(time, securityId)
        preferredDividends <- preferredDividends(time, securityId).orElse(Some(BigDecimal(0)))
        averageSharesOutstanding <- basicWeightedSharesOutstanding(time, securityId)
      } yield (netIncome - preferredDividends) / averageSharesOutstanding
    }

    def basicMarketCapitalization(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
      for {
        price <- adjEodClose(time, securityId)
        averageSharesOutstanding <- basicWeightedSharesOutstanding(time, securityId)
      } yield price * averageSharesOutstanding
    }

    def basicWeightedSharesOutstanding(quarterlyReport: QuarterlyReport): Option[BigDecimal] =
      numericQuarterlyAttribute(quarterlyReport, StatementType.BalanceSheet, BalanceSheetAttributes.BasicWeightedShares)

    def basicWeightedSharesOutstanding(time: DateTime, securityId: SecurityId): Option[BigDecimal] =
      numericQuarterlyAttribute(time, securityId, StatementType.BalanceSheet, BalanceSheetAttributes.BasicWeightedShares)

    def cashAndCashEquivalents(time: DateTime, securityId: SecurityId): Option[BigDecimal] =
      numericQuarterlyAttribute(time, securityId, StatementType.BalanceSheet, BalanceSheetAttributes.CashAndCashEquivalents)

    def currentAssets(time: DateTime, securityId: SecurityId): Option[BigDecimal] =
      numericQuarterlyAttribute(time, securityId, StatementType.BalanceSheet, BalanceSheetAttributes.CurrentAssets)

    def currentLiabilities(time: DateTime, securityId: SecurityId): Option[BigDecimal] =
      numericQuarterlyAttribute(time, securityId, StatementType.BalanceSheet, BalanceSheetAttributes.CurrentLiabilities)

    def dilutedEarningsPerShare(quarterlyReport: QuarterlyReport): Option[BigDecimal] = {
      for {
        netIncome <- netIncome(quarterlyReport)
        preferredDividends <- preferredDividends(quarterlyReport).orElse(Some(BigDecimal(0)))
        averageSharesOutstanding <- dilutedWeightedSharesOutstanding(quarterlyReport)
      } yield (netIncome - preferredDividends) / averageSharesOutstanding
    }

    def dilutedEarningsPerShare(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
      for {
        netIncome <- netIncome(time, securityId)
        preferredDividends <- preferredDividends(time, securityId).orElse(Some(BigDecimal(0)))
        averageSharesOutstanding <- dilutedWeightedSharesOutstanding(time, securityId)
      } yield (netIncome - preferredDividends) / averageSharesOutstanding
    }

    def dilutedMarketCapitalization(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
      for {
        price <- adjEodClose(time, securityId)
        averageSharesOutstanding <- dilutedWeightedSharesOutstanding(time, securityId)
      } yield price * averageSharesOutstanding
    }

    def dilutedWeightedSharesOutstanding(quarterlyReport: QuarterlyReport): Option[BigDecimal] =
      numericQuarterlyAttribute(quarterlyReport, StatementType.BalanceSheet, BalanceSheetAttributes.DilutedWeightedShares)

    def dilutedWeightedSharesOutstanding(time: DateTime, securityId: SecurityId): Option[BigDecimal] =
      numericQuarterlyAttribute(time, securityId, StatementType.BalanceSheet, BalanceSheetAttributes.DilutedWeightedShares)

    def ebit(time: DateTime, securityId: SecurityId): Option[BigDecimal] =
      numericQuarterlyAttribute(time, securityId, StatementType.IncomeStatement, IncomeStatementAttributes.EBIT)

    // http://www.investopedia.com/terms/e/enterprisevalue.asp
    // http://en.wikipedia.org/wiki/Enterprise_value
    // http://www.dailyfinance.com/2011/09/23/learning-mathanese-how-to-calculate-market-cap-and-enterprise-v/
    // market cap + debt - cash and cash equivalents
    def enterpriseValue(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
      for {
        marketCap <- marketCapitalization(time, securityId)
        totalDebt <- totalDebt(time, securityId)
        cash <- cashAndCashEquivalents(time, securityId)
      } yield marketCap + totalDebt - cash
    }

    def greenblattEarningsYield(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
      for {
        ebit <- ebit(time, securityId)
        enterpriseValue <- enterpriseValue(time, securityId)
      } yield ebit / enterpriseValue
    }

    def greenblattReturnOnCapital(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
      for {
        ebit <- ebit(time, securityId)
        netWorkingCapital <- workingCapital(time, securityId)
        netFixedAssets <- netFixedAssets(time, securityId)
      } yield ebit / (netWorkingCapital + netFixedAssets)
    }

    def longTermDebt(time: DateTime, securityId: SecurityId): Option[BigDecimal] =
      numericQuarterlyAttribute(time, securityId, StatementType.BalanceSheet, BalanceSheetAttributes.LongTermDebt)

    def marketCapitalization(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
      for {
        price <- adjEodClose(time, securityId)
        sharesOutstanding <- sharesOutstanding(time, securityId)
      } yield price * sharesOutstanding
    }

    def minorityInterest(time: DateTime, securityId: SecurityId): Option[BigDecimal] =
      numericQuarterlyAttribute(time, securityId, StatementType.IncomeStatement, IncomeStatementAttributes.MinorityInterest)

    def netFixedAssets(time: DateTime, securityId: SecurityId): Option[BigDecimal] = 
      numericQuarterlyAttribute(time, securityId, StatementType.BalanceSheet, BalanceSheetAttributes.NetFixedAssets)

    def netIncome(quarterlyReport: QuarterlyReport): Option[BigDecimal] =
      numericQuarterlyAttribute(quarterlyReport, StatementType.IncomeStatement, IncomeStatementAttributes.NetIncome)

    def netIncome(time: DateTime, securityId: SecurityId): Option[BigDecimal] =
      numericQuarterlyAttribute(time, securityId, StatementType.IncomeStatement, IncomeStatementAttributes.NetIncome)

    def preferredDividends(quarterlyReport: QuarterlyReport): Option[BigDecimal] =
      numericQuarterlyAttribute(quarterlyReport, StatementType.IncomeStatement, IncomeStatementAttributes.PreferredDividends)

    def preferredDividends(time: DateTime, securityId: SecurityId): Option[BigDecimal] =
      numericQuarterlyAttribute(time, securityId, StatementType.IncomeStatement, IncomeStatementAttributes.PreferredDividends)

    // def sharesOutstanding(time: DateTime, quarterlyReport: QuarterlyReport): Option[BigDecimal] = {
    //   val sharesOutAttribute = quarterlyReport.balanceSheet.get(BalanceSheetAttributes.SharesOutstanding)
    //   val qty = sharesOutAttribute match {
    //     case Some(NumericAttribute(shareCount)) => Option(shareCount)
    //     case _ => None
    //   }
    //   qty.map(qty => adjustShareQtyForCorporateActions(qty, quarterlyReport.securityId, quarterlyReport.publicationTime, time))
    // }

    def sharesOutstanding(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
      // val quarterlyReport = findQuarterlyReport(time, securityId)
      // quarterlyReport.flatMap { quarterlyReport =>
      //   val sharesOutAttribute = quarterlyReport.balanceSheet.get(BalanceSheetAttributes.SharesOutstanding)
      //   sharesOutAttribute.flatMap {
      //     case NumericAttribute(shareCount) => Some(adjustShareQtyForCorporateActions(shareCount, securityId, quarterlyReport.publicationTime, time))
      //     case _ => None
      //   }
      // }
      for {
        quarterlyReport <- findQuarterlyReport(time, securityId)
        NumericAttribute(shareCount) <- quarterlyReport.balanceSheet.get(BalanceSheetAttributes.SharesOutstanding)
      } yield adjustShareQtyForCorporateActions(shareCount, securityId, quarterlyReport.publicationTime, time)
    }

    def shortTermDebt(time: DateTime, securityId: SecurityId): Option[BigDecimal] =
      numericQuarterlyAttribute(time, securityId, StatementType.BalanceSheet, BalanceSheetAttributes.ShortTermDebt)

    def totalDebt(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
      for {
        shortTermDebt <- shortTermDebt(time, securityId)
        longTermDebt <- longTermDebt(time, securityId)
      } yield shortTermDebt + longTermDebt
    }
    
    def unadjustedSharesOutstanding(quarterlyReport: QuarterlyReport): Option[BigDecimal] =
      numericQuarterlyAttribute(quarterlyReport, StatementType.BalanceSheet, BalanceSheetAttributes.SharesOutstanding)

    def unadjustedSharesOutstanding(time: DateTime, securityId: SecurityId): Option[BigDecimal] =
      numericQuarterlyAttribute(time, securityId, StatementType.BalanceSheet, BalanceSheetAttributes.SharesOutstanding)

    // http://www.investopedia.com/terms/w/workingcapital.asp
    // working capital = current assets - current liabilities
    def workingCapital(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
      for {
        currentAssets <- currentAssets(time, securityId)
        currentLiabilities <- currentLiabilities(time, securityId)
      } yield currentAssets - currentLiabilities
    }
    
  }

  object ttm {

    def basicEarningsPerShare(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
      for {
        lastFourQuarterlyReports <- findQuarterlyReports(time, securityId, 4)
        lastFourQuarterlyNetIncomeNumbers = lastFourQuarterlyReports.map(report => mrq.netIncome(report))
        if lastFourQuarterlyNetIncomeNumbers.length == 4
        netIncome = lastFourQuarterlyNetIncomeNumbers.flatten.reduceLeft(_ + _)
        preferredDividends <- numericQuarterlyAttribute(time, securityId, StatementType.IncomeStatement, IncomeStatementAttributes.PreferredDividends)
        averageSharesOutstanding <- mrq.basicWeightedSharesOutstanding(time, securityId)
      } yield (netIncome - preferredDividends) / averageSharesOutstanding
    }

    def dilutedEarningsPerShare(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
      for {
        lastFourQuarterlyReports <- findQuarterlyReports(time, securityId, 4)
        lastFourQuarterlyNetIncomeNumbers = lastFourQuarterlyReports.map(report => mrq.netIncome(report))
        if lastFourQuarterlyNetIncomeNumbers.length == 4
        netIncome = lastFourQuarterlyNetIncomeNumbers.flatten.reduceLeft(_ + _)
        preferredDividends <- numericQuarterlyAttribute(time, securityId, StatementType.IncomeStatement, IncomeStatementAttributes.PreferredDividends)
        averageSharesOutstanding <- mrq.dilutedWeightedSharesOutstanding(time, securityId)
      } yield (netIncome - preferredDividends) / averageSharesOutstanding
    }
    
    // see http://www.investopedia.com/terms/p/price-earningsratio.asp
    def basicPriceToEarnings(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
      for {
        price <- adjEodClose(time, securityId)
        lastFourQuarterlyReports <- findQuarterlyReports(time, securityId, 4)
        lastFourQuarterlyEpsNumbers = lastFourQuarterlyReports.map(report => mrq.basicEarningsPerShare(report))
        if lastFourQuarterlyEpsNumbers.length == 4
        eps = lastFourQuarterlyEpsNumbers.flatten.reduceLeft(_ + _)
      } yield price / eps
    }

    def dilutedPriceToEarnings(time: DateTime, securityId: SecurityId): Option[BigDecimal] = {
      for {
        price <- adjEodClose(time, securityId)
        lastFourQuarterlyReports <- findQuarterlyReports(time, securityId, 4)
        lastFourQuarterlyEpsNumbers = lastFourQuarterlyReports.map(report => mrq.dilutedEarningsPerShare(report))
        if lastFourQuarterlyEpsNumbers.length == 4
        eps = lastFourQuarterlyEpsNumbers.flatten.reduceLeft(_ + _)
      } yield price / eps
    }

  }
}

// todo:
// 1. select subset of companies that meet some criteria at time t.
//    > companies.filter { totalDebt(datetime(), _.securityId) == 0 }
// 2. rank companies by some set of attributes
//    > companies.sortBy { totalDebt(datetime(), _.securityId) }    // one attribute
// 3. build company snapshots containing a given set of attributes along side a set of future return yields for each of several different time frames s.t. each timeframe begins at the time the snapshot is taken (or immediately following?)