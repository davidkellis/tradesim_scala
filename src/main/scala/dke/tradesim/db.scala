package dke.tradesim

import scala.slick.driver.PostgresDriver.simple._
import scala.slick.lifted.Query
import scala.slick.session.Database
import scala.slick.jdbc.{GetResult, StaticQuery => Q}

import dke.tradesim.core._
import dke.tradesim.datetimeUtils.{timestamp, datetime, Datestamp, Timestamp}
import dke.tradesim.logger.{verbose, info}
import org.joda.time.DateTime
import scala.util.DynamicVariable

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import org.json4s.JsonDSL.WithBigDecimal._

//import org.apache.thrift.{TBase, TSerializer, TDeserializer}
//import org.apache.thrift.protocol.TBinaryProtocol

import dke.tradesim.core._
import dke.tradesim.json.LiftJValueWithFilter

import scala.language.implicitConversions
import scala.collection.JavaConversions._

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
    implicit def threadLocalAdapter: Adapter = {
      val s = dyn.value
      if(s eq null)
        throw new Exception("No implicit adapter available; threadLocalAdapter can only be used within a withAdapter block")
      else s
    }
  }

  trait Adapter {
    def createDb(): Unit

    def findExchanges(exchangeLabels: Seq[String]): Seq[Exchange]
    def findStocks(exchanges: Seq[Exchange], symbols: Seq[String]): Seq[Security]

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

    def insertTrials[StateT <: State[StateT]](strategyName: String, trialStatePairs: Seq[(Trial, StateT)]): Unit
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


    object Exchanges extends Table[Exchange]("exchanges") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def label = column[String]("label")
      def name = column[Option[String]]("name", O.Nullable)

      // Every table needs a * projection with the same type as the table's type parameter
      def * = id.? ~ label ~ name <> (Exchange, Exchange.unapply _)
      def securities = ExchangeToSecurity.filter(_.exchangeId === id).flatMap(_.securityIdFK)
    }


    object Securities extends Table[Security]("securities") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def bbGid = column[String]("bb_gid")
      def bbGcid = column[String]("bb_gcid")
      def kind = column[String]("type")
      def symbol = column[String]("symbol")
      def name = column[String]("name")
      def startDate = column[Option[Datestamp]]("start_date", O.Nullable)
      def endDate = column[Option[Datestamp]]("end_date", O.Nullable)
      def cik = column[Option[Int]]("cik", O.Nullable)
      def isActive = column[Option[Boolean]]("active", O.Nullable)
      def fiscalYearEndDate = column[Option[Int]]("fiscal_year_end_date", O.Nullable)
      def industryId = column[Option[Int]]("industry_id", O.Nullable)
      def sectorId = column[Option[Int]]("sector_id", O.Nullable)

      // Every table needs a * projection with the same type as the table's type parameter
      def * = id.? ~ bbGid ~ bbGcid ~ kind ~ symbol ~ name ~ startDate ~ endDate ~ cik ~ isActive ~ fiscalYearEndDate ~ industryId ~ sectorId <> (Security, Security.unapply _)
      def exchanges = ExchangeToSecurity.filter(_.securityId === id).flatMap(_.exchangeIdFK)
    }


    // this is a join table between exchanges and securities
    object ExchangeToSecurity extends Table[(Int, Int)]("exchange_securities") {
      def exchangeId = column[Int]("exchange_id")
      def securityId = column[Int]("security_id")

      def * = exchangeId ~ securityId
      def exchangeIdFK = foreignKey("exchange_id_fk", exchangeId, Exchanges)(exchange => exchange.id)
      def securityIdFK = foreignKey("security_id_fk", securityId, Securities)(security => security.id)
    }


    type TrialRecord = (Int, String, String, String, String, String, Timestamp, Timestamp, String, String)
    type NewTrialRecord = (String, String, String, String, String, Timestamp, Timestamp, String, String)
    object Trials extends Table[TrialRecord]("trials") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)   // This is the primary key column
      def strategyName = column[String]("strategy_name")
      def securityIds = column[String]("security_ids")            // This is a JSON array
      def principal = column[String]("principal")
      def commissionPerTrade = column[String]("commission_per_trade")
      def commissionPerShare = column[String]("commission_per_share")
      def startTime = column[Timestamp]("start_time")
      def endTime = column[Timestamp]("end_time")
      def transactionLog = column[String]("transaction_log", O.DBType("text"))
      def portfolioValueLog = column[String]("portfolio_value_log", O.DBType("text"))

      // Every table needs a * projection with the same type as the table's type parameter
      def * = id ~ strategyName ~ securityIds ~ principal ~ commissionPerTrade ~ commissionPerShare ~ startTime ~ endTime ~ transactionLog ~ portfolioValueLog
      def forInsert = strategyName ~ securityIds ~ principal ~ commissionPerTrade ~ commissionPerShare ~ startTime ~ endTime ~ transactionLog ~ portfolioValueLog
    }


    implicit val DateTimeToTimestamp = MappedTypeMapper.base[DateTime, Timestamp](
      dt => timestamp(dt),              // map DateTime to Timestamp
      timestamp => datetime(timestamp)  // map Timestamp to DateTime
    )

    implicit val BigDecimalToString = MappedTypeMapper.base[BigDecimal, String](
      bd => bd.toString,      // map BigDecimal to String
      str => BigDecimal(str)  // map Timestamp to DateTime
    )

    object EodBars extends Table[EodBar]("eod_bars") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)   // This is the primary key column
      def securityId = column[SecurityId]("security_id")
      def startTime = column[DateTime]("start_time")
      def endTime = column[DateTime]("end_time")
      def open = column[BigDecimal]("open")
      def high = column[BigDecimal]("high")
      def low = column[BigDecimal]("low")
      def close = column[BigDecimal]("close")
      def volume = column[Long]("volume")

      // Every table needs a * projection with the same type as the table's type parameter
      def * = id.? ~ securityId ~ startTime ~ endTime ~ open ~ high ~ low ~ close ~ volume <> (EodBar, EodBar.unapply _)
    }

//    def convertEodBarRecord(record: EodBarRecord): EodBar = {
//      record match {
//        case (_, symbol, startTime, endTime, open, high, low, close, volume) =>
//          EodBar(symbol, datetime(startTime), datetime(endTime), BigDecimal(open), BigDecimal(high), BigDecimal(low), BigDecimal(close), volume)
//      }
//    }
//
//    def convertEodBarRecord(record: Option[EodBarRecord]): Option[EodBar] = record.map(convertEodBarRecord(_))


    type CorporateActionRecord = (Int, String, Int, Option[Datestamp], Datestamp, Option[Datestamp], Option[Datestamp], String)
    object CorporateActions extends Table[CorporateActionRecord]("corporate_actions") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)   // This is the primary key column
      def kind = column[String]("type")                     // either Split or CashDividend
      def securityId = column[SecurityId]("security_id")

      def declarationDate = column[Option[Datestamp]]("declaration_date", O.Nullable)
      def exDate = column[Datestamp]("ex_date")
      def recordDate = column[Option[Datestamp]]("record_date", O.Nullable)
      def payableDate = column[Option[Datestamp]]("payable_date", O.Nullable)

      def ratioOrAmount = column[String]("number")      // split ratio OR dividend amount

      // Every table needs a * projection with the same type as the table's type parameter
      def * = id ~ kind ~ securityId ~ declarationDate ~ exDate ~ recordDate ~ payableDate ~ ratioOrAmount
    }

    def convertCorporateActionRecord(record: CorporateActionRecord): CorporateAction = {
      record match {
        case (_, "Split", securityId, _, exDate, _, _, ratio) =>
          Split(securityId, datetime(exDate), BigDecimal(ratio))
        case (_, "CashDividend", securityId, declarationDate, exDate, recordDate, payableDate, amount) =>
          CashDividend(securityId, declarationDate.map(datetime _), datetime(exDate), recordDate.map(datetime _), payableDate.map(datetime _), BigDecimal(amount))
        case (id, _, _, _, _, _, _, _) => throw new Exception(s"Malformed CorporateAction: id=$id")
      }
    }


    type QuarterlyReportRecord = (Int, SecurityId, Timestamp, Timestamp, Timestamp, String, String, String)
    object QuarterlyReports extends Table[QuarterlyReportRecord]("quarterly_reports") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)   // This is the primary key column
      def securityId = column[SecurityId]("security_id")
      def startTime = column[Timestamp]("start_time")
      def endTime = column[Timestamp]("end_time")
      def publicationTime = column[Timestamp]("publication_time")
      def incomeStatement = column[String]("income_statement", O.DBType("text"))
      def balanceSheet = column[String]("balance_sheet", O.DBType("text"))
      def cashFlowStatement = column[String]("cash_flow_statement", O.DBType("text"))

      // Every table needs a * projection with the same type as the table's type parameter
      def * = id ~ securityId ~ startTime ~ endTime ~ publicationTime ~ incomeStatement ~ balanceSheet ~ cashFlowStatement
    }

    type AnnualReportRecord = (Int, SecurityId, Timestamp, Timestamp, Timestamp, String, String, String)
    object AnnualReports extends Table[AnnualReportRecord]("annual_reports") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)   // This is the primary key column
      def securityId = column[SecurityId]("security_id")
      def startTime = column[Timestamp]("start_time")
      def endTime = column[Timestamp]("end_time")
      def publicationTime = column[Timestamp]("publication_time")
      def incomeStatement = column[String]("income_statement", O.DBType("text"))
      def balanceSheet = column[String]("balance_sheet", O.DBType("text"))
      def cashFlowStatement = column[String]("cash_flow_statement", O.DBType("text"))

      // Every table needs a * projection with the same type as the table's type parameter
      def * = id ~ securityId ~ startTime ~ endTime ~ publicationTime ~ incomeStatement ~ balanceSheet ~ cashFlowStatement
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

    def convertQuarterlyReportRecord(record: QuarterlyReportRecord): QuarterlyReport = {
      record match {
        case (_, securityId, startTime, endTime, publicationTime, incomeStatement, balanceSheet, cashFlowStatement) =>
          QuarterlyReport(
            securityId,
            datetime(startTime),
            datetime(endTime),
            datetime(publicationTime),
            convertJsonToStatement(incomeStatement),
            convertJsonToStatement(balanceSheet),
            convertJsonToStatement(cashFlowStatement)
          )
      }
    }

    def convertQuarterlyReportRecord(record: Option[QuarterlyReportRecord]): Option[QuarterlyReport] =
      record.map(convertQuarterlyReportRecord(_))

    def convertAnnualReportRecord(record: AnnualReportRecord): AnnualReport = {
      record match {
        case (_, securityId, startTime, endTime, publicationTime, incomeStatement, balanceSheet, cashFlowStatement) =>
          AnnualReport(
            securityId,
            datetime(startTime),
            datetime(endTime),
            datetime(publicationTime),
            convertJsonToStatement(incomeStatement),
            convertJsonToStatement(balanceSheet),
            convertJsonToStatement(cashFlowStatement)
          )
      }
    }

    def convertAnnualReportRecord(record: Option[AnnualReportRecord]): Option[AnnualReport] =
      record.map(convertAnnualReportRecord(_))


    def withAdapter[T](jdbcConnectionString: String, driver: String, username: String = null, password: String = null)(f: => T): T = {
//      Database.forURL(jdbcConnectionString, username, password, driver = driver) withSession { session =>
//        val adapter = new SlickAdapter()(session)
//        Adapter.withAdapter(adapter)(f)
//      }
      Database.forURL(jdbcConnectionString, username, password, driver = driver) withSession {
        val adapter = new SlickAdapter()(Database.threadLocalSession)
        Adapter.withAdapter(adapter)(f)
      }
    }
  }

  class SlickAdapter(implicit val session: Session) extends Adapter {
    import SlickAdapter._

    def createDb() {
      val ddl: scala.slick.lifted.DDL = EodBars.ddl ++ CorporateActions.ddl ++ QuarterlyReports.ddl ++ AnnualReports.ddl
      ddl.create
    }


    def findExchanges(exchangeLabels: Seq[String]): Seq[Exchange] = {
      Query(Exchanges).
        filter(_.label inSetBind exchangeLabels).
        list
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
                                                             r.nextIntOption))

    def findStocks(exchanges: Seq[Exchange], symbols: Seq[String]): Seq[Security] = {
      val sql = s"""
        |select s.*
        |from securities s
        |inner join exchange_securites etos on etos.security_id = s.id
        |inner join exchanges e on e.id = etos.exchange_id
        |where s.symbol in (${symbols.mkString("'", "','", "'")})
        |  and e.id in (${exchanges.map(_.id).mkString("'", "','", "'")})
      """.stripMargin
      Q.queryNA[Security](sql).list
    }


    def queryEodBar(time: DateTime, securityId: SecurityId): Option[Bar] = {
      val bars = Query(EodBars).filter(_.securityId === securityId).filter(_.startTime <= time)
      val sortedBars = bars.sortBy(_.startTime.desc)
      sortedBars.take(1).firstOption
    }

    def queryEodBarPriorTo(time: DateTime, securityId: SecurityId): Option[Bar] = {
      val bars = Query(EodBars).filter(_.securityId === securityId).filter(_.endTime < time)
      val sortedBars = bars.sortBy(_.endTime.desc)
      sortedBars.take(1).firstOption
    }

    def queryEodBars(securityId: SecurityId): Seq[Bar] = {
      val bars = Query(EodBars).filter(_.securityId === securityId)
      bars.sortBy(_.startTime).list
    }

    def queryEodBars(securityId: SecurityId, earliestTime: DateTime, latestTime: DateTime): Seq[Bar] = {
      val bars = Query(EodBars).filter(_.securityId === securityId)
                               .filter(_.startTime >= earliestTime)
                               .filter(_.endTime <= latestTime)
      bars.sortBy(_.startTime).list
    }

    def findOldestEodBar(securityId: SecurityId): Option[Bar] = {
      val bars = Query(EodBars).filter(_.securityId === securityId)
      val sortedBars = bars.sortBy(_.startTime)
      sortedBars.take(1).firstOption
    }

    def findMostRecentEodBar(securityId: SecurityId): Option[Bar] = {
      val bars = Query(EodBars).filter(_.securityId === securityId)
      val sortedBars = bars.sortBy(_.startTime.desc)
      sortedBars.take(1).firstOption
    }


    implicit val getCorporateActionResult = GetResult(a => (a.nextInt, a.nextString, a.nextString, a.nextInt, a.nextInt, a.nextInt, a.nextInt, a.nextBigDecimal))

    def queryCorporateActions(securityIds: IndexedSeq[Int]): IndexedSeq[CorporateAction] = {
      val sql = s"""
        |select * from corporate_actions
        |where security_id in (${securityIds.mkString("'", "','", "'")})
        |order by ex_date
      """.stripMargin
      Q.queryNA[CorporateActionRecord](sql).mapResult(convertCorporateActionRecord(_)).to[Vector]
    }

    def queryCorporateActions(securityIds: IndexedSeq[Int], startTime: DateTime, endTime: DateTime): IndexedSeq[CorporateAction] = {
      val sql = s"""
        |select * from corporate_actions
        |where security_id in (${securityIds.mkString("'", "','", "'")})
        |and ex_date >= ${timestamp(startTime)}
        |and ex_date <= ${timestamp(endTime)}
        |order by ex_date
      """.stripMargin
      Q.queryNA[CorporateActionRecord](sql).mapResult(convertCorporateActionRecord(_)).to[Vector]
    }


    def queryQuarterlyReport(time: DateTime, securityId: SecurityId): Option[QuarterlyReport] = {
      val reports = Query(QuarterlyReports).filter(_.securityId === securityId).filter(_.startTime <= timestamp(time))
      val sortedReports = reports.sortBy(_.startTime.desc)
      val record = sortedReports.take(1).firstOption
      convertQuarterlyReportRecord(record)
    }

    def queryQuarterlyReportPriorTo(time: DateTime, securityId: SecurityId): Option[QuarterlyReport] = {
      val reports = Query(QuarterlyReports).filter(_.securityId === securityId).filter(_.endTime < timestamp(time))
      val sortedReports = reports.sortBy(_.endTime.desc)
      val record = sortedReports.take(1).firstOption
      convertQuarterlyReportRecord(record)
    }

    def queryQuarterlyReports(securityId: SecurityId): Seq[QuarterlyReport] = {
      val reports = Query(QuarterlyReports).filter(_.securityId === securityId)
      val sortedReports = reports.sortBy(_.startTime)
      sortedReports.mapResult(convertQuarterlyReportRecord(_)).list
    }

    def queryQuarterlyReports(securityId: SecurityId, earliestTime: DateTime, latestTime: DateTime): Seq[QuarterlyReport] = {
      val reports = Query(QuarterlyReports).filter(_.securityId === securityId)
                                           .filter(_.startTime >= timestamp(earliestTime))
                                           .filter(_.endTime <= timestamp(latestTime))
      val sortedReports = reports.sortBy(_.startTime)
      sortedReports.mapResult(convertQuarterlyReportRecord(_)).list
    }


    def queryAnnualReport(time: DateTime, securityId: SecurityId): Option[AnnualReport] = {
      val reports = Query(AnnualReports).filter(_.securityId === securityId).filter(_.startTime <= timestamp(time))
      val sortedReports = reports.sortBy(_.startTime.desc)
      val record = sortedReports.take(1).firstOption
      convertAnnualReportRecord(record)
    }

    def queryAnnualReportPriorTo(time: DateTime, securityId: SecurityId): Option[AnnualReport] = {
      val reports = Query(AnnualReports).filter(_.securityId === securityId).filter(_.endTime < timestamp(time))
      val sortedReports = reports.sortBy(_.endTime.desc)
      val record = sortedReports.take(1).firstOption
      convertAnnualReportRecord(record)
    }

    def queryAnnualReports(securityId: SecurityId): Seq[AnnualReport] = {
      val reports = Query(AnnualReports).filter(_.securityId === securityId)
      val sortedReports = reports.sortBy(_.startTime)
      sortedReports.mapResult(convertAnnualReportRecord(_)).list
    }

    def queryAnnualReports(securityId: SecurityId, earliestTime: DateTime, latestTime: DateTime): Seq[AnnualReport] = {
      val reports = Query(AnnualReports).filter(_.securityId === securityId)
                                        .filter(_.startTime >= timestamp(earliestTime))
                                        .filter(_.endTime <= timestamp(latestTime))
      val sortedReports = reports.sortBy(_.startTime)
      sortedReports.mapResult(convertAnnualReportRecord(_)).list
    }


    // Trial stuff

    def insertTrials[StateT <: State[StateT]](strategyName: String, trialStatePairs: Seq[(Trial, StateT)]): Unit = {
      // trialStatePairs.foreach { pair => 
      //   Trials.forInsert.insert(buildInsertionTuple(strategyName, pair._1, pair._2))
      // }
      trialStatePairs.grouped(500).foreach { pairs =>
        verbose(s"Building group of records.")
        val records = pairs.map(pair => buildInsertionTuple(strategyName, pair._1, pair._2)).toSeq
        verbose(s"Inserting group of records.")
        Trials.forInsert.insertAll(records: _*)
      }
    }

    def buildInsertionTuple[StateT <: State[StateT]](strategyName: String, trial: Trial, state: StateT): NewTrialRecord = {
      val securityIds = Serialization.write(SecurityIds(trial.securityIds))
      val principal = trial.principal.toString
      val commissionPerTrade = trial.commissionPerTrade.toString
      val commissionPerShare = trial.commissionPerShare.toString
      val startTime = timestamp(trial.startTime)
      val endTime = timestamp(trial.endTime)
      val transactionLog = Serialization.write(Transactions(state.transactions))
      val portfolioValueLog = Serialization.write(PortfolioValues(state.portfolioValueHistory))

      (strategyName, securityIds, principal, commissionPerTrade, commissionPerShare, startTime, endTime, transactionLog, portfolioValueLog)
    }
  }
}