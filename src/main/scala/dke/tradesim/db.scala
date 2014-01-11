package dke.tradesim

import scala.slick.driver.PostgresDriver.simple._
import scala.slick.jdbc.{GetResult, StaticQuery => Q}
import javax.sql.rowset.serial.SerialBlob

import dke.tradesim.core._
import dke.tradesim.database.Tables._
import dke.tradesim.datetimeUtils.{timestamp, datestamp, datetime, date, Datestamp, Timestamp}
import dke.tradesim.logger.{verbose, info}
import org.joda.time.DateTime
import scala.util.DynamicVariable

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import org.json4s.JsonDSL.WithBigDecimal._

import dke.tradesim.json.LiftJValueWithFilter

//import scala.collection.JavaConversions._

object db {
  object Adapter {
    val dyn = new DynamicVariable[Adapter](null)

    /**
     * Run the supplied thunk with a new adapter and automatically close the session at the end.
     * The session is stored in a thread-local variable which can be accessed with the implicit
     * function in Database.Implicit.
     */
    def withAdapter[T](adapter: Adapter)(f: => T): T = dyn.withValue(adapter)(f)

    /**
     * An implicit function that returns the thread-local adapter in a withAdapater block
     */
    implicit def dynamicAdapter: Adapter = {
      val s = dyn.value
      if(s eq null)
        throw new Exception("No implicit adapter available; dynamicAdapter can only be used within a withAdapter block")
      else s
    }
  }

  trait Adapter {
    def createDb(): Unit

    def findExchanges(exchangeLabels: Seq[String]): Seq[Exchange]
    def findSecurities(exchanges: Seq[Exchange], symbols: Seq[String]): Seq[Security]

    def queryEodBar(time: DateTime, securityId: SecurityId): Option[Bar]
    def queryEodBarPriorTo(time: DateTime, securityId: SecurityId): Option[Bar]
    def queryEodBars(securityId: SecurityId): Seq[Bar]
    def queryEodBars(securityId: SecurityId, earliestTime: DateTime, latestTime: DateTime): Seq[Bar]
    def findOldestEodBar(securityId: SecurityId): Option[Bar]
    def findMostRecentEodBar(securityId: SecurityId): Option[Bar]

    def queryCorporateActions(securityIds: IndexedSeq[Int]): IndexedSeq[CorporateAction]
    def queryCorporateActions(securityIds: IndexedSeq[Int], startTime: DateTime, endTime: DateTime): IndexedSeq[CorporateAction]

    def queryQuarterlyReport(time: DateTime, securityId: SecurityId): Option[QuarterlyReport]
    def queryQuarterlyReportPriorTo(time: DateTime, securityId: SecurityId): Option[QuarterlyReport]
    def queryQuarterlyReports(securityId: SecurityId): Seq[QuarterlyReport]
    def queryQuarterlyReports(securityId: SecurityId, earliestTime: DateTime, latestTime: DateTime): Seq[QuarterlyReport]

    def queryAnnualReport(time: DateTime, securityId: SecurityId): Option[AnnualReport]
    def queryAnnualReportPriorTo(time: DateTime, securityId: SecurityId): Option[AnnualReport]
    def queryAnnualReports(securityId: SecurityId): Seq[AnnualReport]
    def queryAnnualReports(securityId: SecurityId, earliestTime: DateTime, latestTime: DateTime): Seq[AnnualReport]

    def insertTrials[StateT <: State[StateT]](strategy: Strategy[StateT], trialStatePairs: Seq[(Trial, StateT)]): Unit
  }

  object SlickAdapter {
    class DateTimeSerializer extends CustomSerializer[DateTime](format => (
      {
        case JObject(JField("timestamp", JInt(timestamp)) :: Nil) => datetime(timestamp.toLong)
      },
      {
        case t: DateTime => JObject(JField("timestamp", JInt(timestamp(t))) :: Nil)
      }
      )
    )

    implicit val formats = Serialization.formats(ShortTypeHints(List(
      classOf[MarketBuy],
      classOf[MarketSell],
      classOf[LimitBuy],
      classOf[LimitSell],
      classOf[SplitAdjustment],
      classOf[CashDividendPayment]
    ))) + new DateTimeSerializer

//    implicit val formats = Serialization.formats(NoTypeHints) + new DateTimeSerializer


    def convertExchangesRow(record: ExchangesRow): Exchange = {
      record match {
        case ExchangesRow(id, label, name) =>
          Exchange(Some(id), label, name)
      }
    }

    def convertSecuritiesRow(record: SecuritiesRow): Security = {
      record match {
        case SecuritiesRow(id, kind, Some(bbGid), Some(bbGcid), symbol, Some(name), startDate, endDate, cik, fiscalYearEndDate, active, exchangeId, industryId, sectorId) =>
          Security(Some(id), bbGid, bbGcid, kind, symbol, name, startDate, endDate, cik, active, fiscalYearEndDate, exchangeId, industryId, sectorId)
        case SecuritiesRow(id, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
          throw new Exception(s"Malformed SecuritiesRow: id=$id")
      }
    }

    implicit val DateTimeToTimestamp = MappedColumnType.base[DateTime, Timestamp](
      dt => timestamp(dt),              // map DateTime to Timestamp
      timestamp => datetime(timestamp)  // map Timestamp to DateTime
    )

    implicit val BigDecimalToString = MappedColumnType.base[BigDecimal, String](
      bd => bd.toString,      // map BigDecimal to String
      str => BigDecimal(str)  // map Timestamp to DateTime
    )

    def convertEodBarsRow(record: EodBarsRow): EodBar = {
      record match {
        case EodBarsRow(id, startTime, endTime, open, high, low, close, volume, securityId) =>
          EodBar(Some(id), securityId, datetime(startTime), datetime(endTime), open, high, low, close, volume)
      }
    }

    def convertEodBarsRow(record: Option[EodBarsRow]): Option[EodBar] = record.map(convertEodBarsRow(_))

    def convertCorporateActionsRow(record: CorporateActionsRow): CorporateAction = {
      record match {
        case CorporateActionsRow(id, "Split", exDate, declarationDate, recordDate, payableDate, ratio, securityId) =>
          Split(securityId, date(exDate), ratio)
        case CorporateActionsRow(id, "CashDividend", exDate, declarationDate, recordDate, payableDate, amount, securityId) =>
          CashDividend(securityId, declarationDate.map(date _), date(exDate), recordDate.map(date _), payableDate.map(date _), amount)
        case CorporateActionsRow(id, kind, exDate, declarationDate, recordDate, payableDate, ratio, securityId) =>
          throw new Exception(s"Malformed CorporateAction: id=$id")
      }
    }

    // assumes json is of the form: [ "header", [attribute, value], [attribute, value], "header", [attr, val], ... ]
    // where the json structure is no more than 1 level deep (i.e. there are no sub-sections)
    def convertJsonToStatement(jsonString: String): Statement = {
      val pairs = for {
        JObject(attributes) <- parse(jsonString, useBigDecimalForDouble = true)
        keyValuePair <- attributes
      } yield keyValuePair match {
        case (attributeName, JNull) => (attributeName -> HeaderAttribute(attributeName))
        case (attributeName, attributeValue) => attributeValue match {
          case JString(value) => (attributeName -> StringAttribute(value))
          case JDecimal(value) => (attributeName -> NumericAttribute(value))
          case _ => throw new Exception(s"Unable to parse key/value pair: $attributeName/$attributeValue. The statement is malformed: $jsonString")
        }
        case _ => throw new Exception(s"Unable to parse statement. It is malformed: $jsonString")
      }
      pairs.toMap
    }

    def getString(jValue: JValue): Option[String] = jValue match {
      case JString(str) => Option(str)
      case _ => None
    }

    def convertQuarterlyReportsRow(record: QuarterlyReportsRow): QuarterlyReport = {
      record match {
        case QuarterlyReportsRow(id, startTime, endTime, publicationTime, incomeStatement, balanceSheet, cashFlowStatement, securityId) =>
          QuarterlyReport(
            securityId,
            datetime(startTime),
            datetime(endTime),
            datetime(publicationTime),
            convertJsonToStatement(incomeStatement.toString),     // todo: the .toString is just to make this compile - it is wrong - fix it
            convertJsonToStatement(balanceSheet.toString),        // todo: the .toString is just to make this compile - it is wrong - fix it
            convertJsonToStatement(cashFlowStatement.toString)    // todo: the .toString is just to make this compile - it is wrong - fix it
          )
      }
    }

    def convertQuarterlyReportsRow(record: Option[QuarterlyReportsRow]): Option[QuarterlyReport] =
      record.map(convertQuarterlyReportsRow(_))

    def convertAnnualReportsRow(record: AnnualReportsRow): AnnualReport = {
      record match {
        case AnnualReportsRow(id, startTime, endTime, publicationTime, incomeStatement, balanceSheet, cashFlowStatement, securityId) =>
          AnnualReport(
            securityId,
            datetime(startTime),
            datetime(endTime),
            datetime(publicationTime),
            convertJsonToStatement(incomeStatement.toString),     // todo: the .toString is just to make this compile - it is wrong - fix it
            convertJsonToStatement(balanceSheet.toString),        // todo: the .toString is just to make this compile - it is wrong - fix it
            convertJsonToStatement(cashFlowStatement.toString)    // todo: the .toString is just to make this compile - it is wrong - fix it
          )
      }
    }

    def convertAnnualReportsRow(record: Option[AnnualReportsRow]): Option[AnnualReport] =
      record.map(convertAnnualReportsRow(_))


    def withAdapter[T](jdbcConnectionString: String, driver: String, username: String = null, password: String = null)(f: => T): T = {
      Database.forURL(jdbcConnectionString, username, password, driver = driver) withDynSession {
        val adapter = new SlickAdapter()(Database.dynamicSession)
        Adapter.withAdapter(adapter)(f)
      }
    }
  }

  class SlickAdapter(implicit val session: Session) extends Adapter {
    import SlickAdapter._

    def createDb() {
      val ddl = EodBars.ddl ++ CorporateActions.ddl ++ QuarterlyReports.ddl ++ AnnualReports.ddl
      ddl.create
    }


    def findExchanges(exchangeLabels: Seq[String]): Seq[Exchange] = {
      Exchanges.filter(_.label inSetBind exchangeLabels).list.map(convertExchangesRow _)
    }


    implicit val getSecurityResult = GetResult(r => Security(r.nextIntOption,
                                                             r.nextString,
                                                             r.nextString,
                                                             r.nextString,
                                                             r.nextString,
                                                             r.nextString,
                                                             r.nextIntOption,
                                                             r.nextIntOption,
                                                             r.nextIntOption,
                                                             r.nextBooleanOption,
                                                             r.nextIntOption,
                                                             r.nextIntOption,
                                                             r.nextIntOption,
                                                             r.nextIntOption))

    def findSecurities(exchanges: Seq[Exchange], symbols: Seq[String]): Seq[Security] = {
      val sql = s"""
        |select s.id, s.bb_gid, s.bb_gcid, s.type, s.symbol, s.name, s.start_date, s.end_date, s.cik, s.active, s.fiscal_year_end_date, s.exchange_id, s.industry_id, s.sector_id
        |from securities s
        |inner join exchanges e on e.id = s.exchange_id
        |where s.symbol in (${symbols.mkString("'", "','", "'")})
        |  and e.id in (${exchanges.flatMap(_.id).mkString("'", "','", "'")})
      """.stripMargin
      Q.queryNA[Security](sql).list
    }


    def queryEodBar(time: DateTime, securityId: SecurityId): Option[Bar] = {
      val bars = EodBars.filter(_.securityId === securityId).filter(_.startTime <= timestamp(time))
      val sortedBars = bars.sortBy(_.startTime.desc)
      sortedBars.take(1).firstOption.map(convertEodBarsRow _)
    }

    def queryEodBarPriorTo(time: DateTime, securityId: SecurityId): Option[Bar] = {
      val bars = EodBars.filter(_.securityId === securityId).filter(_.endTime < timestamp(time))
      val sortedBars = bars.sortBy(_.endTime.desc)
      sortedBars.take(1).firstOption.map(convertEodBarsRow _)
    }

    def queryEodBars(securityId: SecurityId): Seq[Bar] = {
      val bars = EodBars.filter(_.securityId === securityId)
      bars.sortBy(_.startTime).list.map(convertEodBarsRow _)
    }

    def queryEodBars(securityId: SecurityId, earliestTime: DateTime, latestTime: DateTime): Seq[Bar] = {
      val bars = EodBars.filter(_.securityId === securityId)
                        .filter(_.startTime >= timestamp(earliestTime))
                        .filter(_.endTime <= timestamp(latestTime))
      bars.sortBy(_.startTime).list.map(convertEodBarsRow _)
    }

    def findOldestEodBar(securityId: SecurityId): Option[Bar] = {
      val bars = EodBars.filter(_.securityId === securityId)
      val sortedBars = bars.sortBy(_.startTime)
      sortedBars.take(1).firstOption.map(convertEodBarsRow _)
    }

    def findMostRecentEodBar(securityId: SecurityId): Option[Bar] = {
      val bars = EodBars.filter(_.securityId === securityId)
      val sortedBars = bars.sortBy(_.startTime.desc)
      sortedBars.take(1).firstOption.map(convertEodBarsRow _)
    }


    implicit val getCorporateActionResult = GetResult(a => (a.nextInt, a.nextString, a.nextString, a.nextInt, a.nextInt, a.nextInt, a.nextInt, a.nextBigDecimal))

    def queryCorporateActions(securityIds: IndexedSeq[Int]): IndexedSeq[CorporateAction] = {
      val sql = s"""
        |select id, type, security_id, declaration_date, ex_date, record_date, payable_date, number from corporate_actions
        |where security_id in (${securityIds.mkString("'", "','", "'")})
        |order by ex_date
      """.stripMargin
      Q.queryNA[CorporateActionsRow](sql).mapResult(convertCorporateActionsRow(_)).buildColl[Vector]
    }

    def queryCorporateActions(securityIds: IndexedSeq[Int], startTime: DateTime, endTime: DateTime): IndexedSeq[CorporateAction] = {
      val sql = s"""
        |select id, type, security_id, declaration_date, ex_date, record_date, payable_date, number from corporate_actions
        |where security_id in (${securityIds.mkString("'", "','", "'")})
        |and ex_date >= ${timestamp(startTime)}
        |and ex_date <= ${timestamp(endTime)}
        |order by ex_date
      """.stripMargin
      Q.queryNA[CorporateActionsRow](sql).mapResult(convertCorporateActionsRow(_)).buildColl[Vector]
    }


    def queryQuarterlyReport(time: DateTime, securityId: SecurityId): Option[QuarterlyReport] = {
      val reports = QuarterlyReports.filter(_.securityId === securityId).filter(_.startTime <= timestamp(time))
      val sortedReports = reports.sortBy(_.startTime.desc)
      val record = sortedReports.take(1).firstOption
      convertQuarterlyReportsRow(record)
    }

    def queryQuarterlyReportPriorTo(time: DateTime, securityId: SecurityId): Option[QuarterlyReport] = {
      val reports = QuarterlyReports.filter(_.securityId === securityId).filter(_.endTime < timestamp(time))
      val sortedReports = reports.sortBy(_.endTime.desc)
      val record = sortedReports.take(1).firstOption
      convertQuarterlyReportsRow(record)
    }

    def queryQuarterlyReports(securityId: SecurityId): Seq[QuarterlyReport] = {
      val reports = QuarterlyReports.filter(_.securityId === securityId)
      val sortedReports = reports.sortBy(_.startTime)
      sortedReports.mapResult(convertQuarterlyReportsRow(_)).list
    }

    def queryQuarterlyReports(securityId: SecurityId, earliestTime: DateTime, latestTime: DateTime): Seq[QuarterlyReport] = {
      val reports = QuarterlyReports.filter(_.securityId === securityId)
                                    .filter(_.startTime >= timestamp(earliestTime))
                                    .filter(_.endTime <= timestamp(latestTime))
      val sortedReports = reports.sortBy(_.startTime)
      sortedReports.mapResult(convertQuarterlyReportsRow(_)).list
    }


    def queryAnnualReport(time: DateTime, securityId: SecurityId): Option[AnnualReport] = {
      val reports = AnnualReports.filter(_.securityId === securityId).filter(_.startTime <= timestamp(time))
      val sortedReports = reports.sortBy(_.startTime.desc)
      val record = sortedReports.take(1).firstOption
      convertAnnualReportsRow(record)
    }

    def queryAnnualReportPriorTo(time: DateTime, securityId: SecurityId): Option[AnnualReport] = {
      val reports = AnnualReports.filter(_.securityId === securityId).filter(_.endTime < timestamp(time))
      val sortedReports = reports.sortBy(_.endTime.desc)
      val record = sortedReports.take(1).firstOption
      convertAnnualReportsRow(record)
    }

    def queryAnnualReports(securityId: SecurityId): Seq[AnnualReport] = {
      val reports = AnnualReports.filter(_.securityId === securityId)
      val sortedReports = reports.sortBy(_.startTime)
      sortedReports.mapResult(convertAnnualReportsRow(_)).list
    }

    def queryAnnualReports(securityId: SecurityId, earliestTime: DateTime, latestTime: DateTime): Seq[AnnualReport] = {
      val reports = AnnualReports.filter(_.securityId === securityId)
                                 .filter(_.startTime >= timestamp(earliestTime))
                                 .filter(_.endTime <= timestamp(latestTime))
      val sortedReports = reports.sortBy(_.startTime)
      sortedReports.mapResult(convertAnnualReportsRow(_)).list
    }


    // Trial stuff

    def insertTrials[StateT <: State[StateT]](strategy: Strategy[StateT], trialStatePairs: Seq[(Trial, StateT)]): Unit = {
      trialStatePairs.headOption.foreach { firstTrialStatePair =>
        val strategyRow = insertStrategy(strategy.name)
        val trialSetRow = insertTrialSet(strategyRow.id, firstTrialStatePair._1)

        trialStatePairs.grouped(500).foreach { pairs =>
          verbose(s"Building group of records.")
          val trialRows = pairs.map(pair => buildTrialsRow(trialSetRow.id, pair._1, pair._2)).toSeq
          verbose(s"Inserting group of records.")
          try {
            Trials ++= trialRows
          } catch {
            case e: java.sql.BatchUpdateException =>
              println("*" * 80)
              e.getNextException.printStackTrace()
          }
        }
      }
    }

    def insertStrategy(strategyName: String): StrategiesRow = {
      val insertRow = () => {
        val row = StrategiesRow(0, strategyName)
        val strategyId = (Strategies returning Strategies.map(_.id)) += row
        row.copy(id = strategyId)
      }
      Strategies.filter(_.name === strategyName).take(1).firstOption.getOrElse(insertRow())
    }

    def insertTrialSet(strategyId: Int, trial: Trial): TrialSetsRow = {
      val insertRow = () => {
        val row = TrialSetsRow(0, Some(trial.principal), Some(trial.commissionPerTrade), Some(trial.commissionPerShare), strategyId)
        val trialSetId = (TrialSets returning TrialSets.map(_.id)).insert(row)
        joinTrialSetToSecurities(trialSetId, trial.securityIds)
        row.copy(id = trialSetId)
      }

      val tsId_sId_pairs = (for {
        ts <- TrialSets if ts.strategyId === strategyId
        sToTs <- SecuritiesTrialSets if sToTs.trialSetId === ts.id
        s <- Securities if sToTs.securityId === s.id
        if s.id inSetBind trial.securityIds
        if ts.principal === trial.principal
        if ts.commissionPerTrade === trial.commissionPerTrade
        if ts.commissionPerShare === trial.commissionPerShare
      } yield (ts.id, s.id)).list

//      val tsId_sId_pairs = (for {
//        ((ts, joinTable), s) <- (TrialSets innerJoin SecuritiesTrialSets) innerJoin Securities
//        if ts.strategyId === strategyId
//        if s.id inSetBind trial.securityIds
//        if ts.principal === trial.principal
//        if ts.commissionPerTrade === trial.commissionPerTrade
//        if ts.commissionPerShare === trial.commissionPerShare
//      } yield (ts.id, s.id)).list

      val groupedIdPairs = tsId_sId_pairs.groupBy(_._1)     // group (tsId, sId) pairs by tsId yielding a Map[tsId, Seq[sId]]

      val groupedIdPairReferencingAllSecurities = groupedIdPairs.find(_._2.length == trial.securityIds.length)

      val trialSetId = groupedIdPairReferencingAllSecurities.map(_._1)

      trialSetId.flatMap { trialSetId =>
        TrialSets.filter(_.id === trialSetId).take(1).firstOption
      }.getOrElse(insertRow())
    }

    def joinTrialSetToSecurities(trialSetId: Int, securityIds: Seq[SecurityId]): Unit = {
      val rows = securityIds.map(securityId => SecuritiesTrialSetsRow(securityId, trialSetId))
      SecuritiesTrialSets ++= rows
    }

    def buildTrialsRow[StateT <: State[StateT]](trialSetId: Int, trial: Trial, state: StateT): TrialsRow = {
      val startTime = timestamp(trial.startTime)
      val endTime = timestamp(trial.endTime)
//      val transactionLog = convertByteArrayToBlob(convertTransactionsToProtobuf(state.transactions).toByteArray)
//      val portfolioValueLog = convertByteArrayToBlob(convertPortfolioValuesToProtobuf(state.portfolioValueHistory).toByteArray)
      val transactionLog = convertTransactionsToProtobuf(state.transactions).toByteArray
      val portfolioValueLog = convertPortfolioValuesToProtobuf(state.portfolioValueHistory).toByteArray

      TrialsRow(0, startTime, endTime, transactionLog, portfolioValueLog, trialSetId)
    }

    def convertTransactionsToProtobuf(transactions: TransactionLog): protobuf.TransactionLog = {
      protobuf.TransactionLog(transactions.map(convertTransactionToProtobuf _).to[collection.immutable.Seq])
    }

    def convertTransactionToProtobuf(transaction: Transaction): protobuf.Transaction = {
      transaction match {
        case order: Order =>
          protobuf.Transaction(protobuf.Transaction.Type.Order, Some(convertOrderToProtobuf(order)), None, None)
        case splitAdjustment: SplitAdjustment =>
          protobuf.Transaction(protobuf.Transaction.Type.SplitAdjustment, None, Some(convertSplitAdjustmentToProtobuf(splitAdjustment)), None)
        case cashDividendPayment: CashDividendPayment =>
          protobuf.Transaction(protobuf.Transaction.Type.CashDividendPayment, None, None, Some(convertCashDividendPaymentToProtobuf(cashDividendPayment)))
      }
    }

    def convertOrderToProtobuf(order: Order): protobuf.Order = {
      order match {
        case MarketBuy(time, securityId, qty, fillPrice) =>
          protobuf.Order(protobuf.Order.Type.MarketBuy, timestamp(time), securityId, qty, fillPrice.map(_.toString), None)
        case MarketSell(time, securityId, qty, fillPrice) =>
          protobuf.Order(protobuf.Order.Type.MarketSell, timestamp(time), securityId, qty, fillPrice.map(_.toString), None)
        case LimitBuy(time, securityId, qty, limitPrice, fillPrice) =>
          protobuf.Order(protobuf.Order.Type.MarketBuy, timestamp(time), securityId, qty, fillPrice.map(_.toString), Some(limitPrice.toString))
        case LimitSell(time, securityId, qty, limitPrice, fillPrice) =>
          protobuf.Order(protobuf.Order.Type.MarketSell, timestamp(time), securityId, qty, fillPrice.map(_.toString), Some(limitPrice.toString))
      }
    }

    def convertSplitAdjustmentToProtobuf(splitAdjustment: SplitAdjustment): protobuf.SplitAdjustment = {
      protobuf.SplitAdjustment(
        splitAdjustment.securityId,
        datestamp(splitAdjustment.exDate),
        splitAdjustment.ratio.toString,
        timestamp(splitAdjustment.adjustmentTime),
        splitAdjustment.shareQtyDelta,
        splitAdjustment.cashPayout.toString
      )
    }

    def convertCashDividendPaymentToProtobuf(cashDividendPayment: CashDividendPayment): protobuf.CashDividendPayment = {
      protobuf.CashDividendPayment(
        cashDividendPayment.securityId,
        datestamp(cashDividendPayment.exDate),
        cashDividendPayment.payableDate.map(datestamp _),
        cashDividendPayment.amountPerShare.toString,
        timestamp(cashDividendPayment.adjustmentTime),
        cashDividendPayment.shareQty,
        cashDividendPayment.total.toString
      )
    }

    def convertPortfolioValuesToProtobuf(portfolioValues: Seq[PortfolioValue]): protobuf.PortfolioValueLog = {
      protobuf.PortfolioValueLog(portfolioValues.map(convertPortfolioValueToProtobuf _).to[collection.immutable.Seq])
    }

    def convertPortfolioValueToProtobuf(portfolioValue: PortfolioValue): protobuf.PortfolioValue = {
      protobuf.PortfolioValue(
        timestamp(portfolioValue.time),
        portfolioValue.value.toString
      )
    }

    def convertByteArrayToBlob(byteArray: Array[Byte]): java.sql.Blob = new SerialBlob(byteArray)
  }
}