package dke.tradesim

import org.joda.time.DateTime

object quarterlyReports {
  class Report {
    val stringAttributes = Map[String, String]()
    val numericAttributes = Map[String, BigDecimal]()

    def getString(key: String): Option[String] = stringAttributes.get(key)
    def getNumber(key: String): Option[BigDecimal] = numericAttributes.get(key)

    def set(key: String, value: String): Report = ???
    def set(key: String, value: BigDecimal): Report = ???
  }

  class IncomeStatement extends Report {

  }

  class BalanceSheet extends Report {

  }

  class CashFlowStatement extends Report {

  }

  case class QuarterlyReport(symbol: String,
                             startTime: DateTime,
                             endTime: DateTime,
                             publicationTime: DateTime,
                             incomeStatement: IncomeStatement,
                             balanceSheet: BalanceSheet,
                             cashFlowStatement: CashFlowStatement)
}