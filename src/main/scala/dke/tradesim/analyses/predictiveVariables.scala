package dke.tradesim.analyses

import org.joda.time.DateTime

import dke.tradesim.core.{Bar, QuarterlyReport, SecurityId, extractNumericAttributes}
import dke.tradesim.{quotes, quarterlyReports}

object predictiveVariables {
  case class AttributeSetSnapshot(attributes: Map[String, BigDecimal], futureReturns: Map[String, BigDecimal])
  case class CompanySnapshot(quarterlyReport: QuarterlyReport, eodBar: Bar)

  def companySnapshot(time: DateTime, securityId: SecurityId): Option[CompanySnapshot] = {
    for {
      quarterlyReport <- quarterlyReports.findQuarterlyReport(time, securityId)
      eodBar <- quotes.findEodBar(time, securityId)
    } yield CompanySnapshot(quarterlyReport, eodBar)
  }

  def snapshotOfAttributes(time: DateTime, securityId: SecurityId): Option[AttributeSetSnapshot] = {
    val snapshot = companySnapshot(time, securityId)
    snapshot.map { snapshot =>
      val attributes = extractNumericAttributes(snapshot.quarterlyReport.incomeStatement) ++
                       extractNumericAttributes(snapshot.quarterlyReport.balanceSheet) ++
                       extractNumericAttributes(snapshot.quarterlyReport.cashFlowStatement)
      // todo, finish this
//      val futureReturns = queryForFutureReturns()
      val futureReturns = Map[String, BigDecimal]()
      AttributeSetSnapshot(attributes, futureReturns)
    }
  }
}
