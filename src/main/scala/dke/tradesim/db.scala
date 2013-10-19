package dke.tradesim

import com.mongodb.casbah.Imports._

import scala.slick.driver.PostgresDriver.simple._
import scala.slick.lifted.Query
import scala.slick.session.Database
import scala.slick.jdbc.{GetResult, StaticQuery => Q}

import dke.tradesim.core._
import dke.tradesim.datetimeUtils.{timestamp, datetime, Datestamp, Timestamp}
import org.joda.time.DateTime
import scala.util.DynamicVariable

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.JValue

import org.apache.thrift.{TBase, TSerializer, TDeserializer}
import org.apache.thrift.protocol.TBinaryProtocol

import dke.tradesim.core.CashDividend
import dke.tradesim.core.EodBar
import dke.tradesim.core.AnnualReport
import dke.tradesim.core.QuarterlyReport
import dke.tradesim.core.Split
import dke.tradesim.json.LiftJValueWithFilter

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

    def insertTrials(strategyName: String, trialStatePairs: Seq[(Trial, State)]): Seq[Int]
  }

//  object MongoAdapter {
//    def withAdapter[T](mongoDbConnectionString: String)(f: => T): T = {
//      val uri = new MongoClientURI(mongoDbConnectionString)
//      val mongoClient = MongoClient(uri)
//      val adapter = new MongoAdapter(mongoClient)
//      Adapter.withAdapter(adapter, f)
//    }
//
//    def convertEodBarRecord(record: MongoDBObject): EodBar = {
//      EodBar(
//        record.as[String]("s"),
//        datetime(record.as[Long]("ts")),
//        datetime(record.as[Long]("te")),
//        record.as[BigDecimal]("o"),
//        record.as[BigDecimal]("h"),
//        record.as[BigDecimal]("l"),
//        record.as[BigDecimal]("c"),
//        record.as[Long]("v")
//      )
//    }
//
//    def convertCorporateActionRecord(record: MongoDBObject): CorporateAction = {
//      val kind = record.as[String]("_type")
//      kind match {
//        case "Split" => convertSplitRecord(record)
//        case "CashDividend" => convertCashDividendRecord(record)
//        case _ => throw new IllegalArgumentException("A corporate action's _type must be either Split or CashDividend")
//      }
//    }
//
//    def convertSplitRecord(record: MongoDBObject): Split = {
//      Split(
//        record.as[String]("s"),
//        datetime(record.as[Long]("ex")),
//        record.as[BigDecimal]("r")
//      )
//    }
//
//    def convertCashDividendRecord(record: MongoDBObject): CashDividend = {
//      CashDividend(
//        record.as[String]("s"),
//        datetime(record.as[Long]("decl")),
//        datetime(record.as[Long]("ex")),
//        datetime(record.as[Long]("rec")),
//        datetime(record.as[Long]("pay")),
//        record.as[BigDecimal]("a")
//      )
//    }
//
//    def convertQuarterlyReportRecord(record: MongoDBObject): QuarterlyReport = {
//      QuarterlyReport(
//        record.as[String]("s"),
//        datetime(record.as[Long]("ts")),  // start time
//        datetime(record.as[Long]("te")),  // end time
//        datetime(record.as[Long]("tp")),  // publication time
//        record.as[BigDecimal]("o"),
//        record.as[BigDecimal]("h"),
//        record.as[BigDecimal]("l"),
//        record.as[BigDecimal]("c"),
//        record.as[Long]("v")
//      )
//    }
//  }
//
//  class MongoAdapter(val mongoClient: MongoClient) extends Adapter {
//    import MongoAdapter._
//
//    def queryEodBar(time: DateTime, securityId: SecurityId): Option[Bar] = {
//      val eodBars = mongoClient("tradesim")("eod_bars")
//      val query: DBObject = MongoDBObject.empty ++ ("s" -> symbol) ++ ("ts" $lte timestamp(time))
//      val orderByClause = MongoDBObject.empty ++ ("ts" -> -1)
//      val cursor = (eodBars.find(query) $orderby orderByClause).limit(1)
//      val obj = Option(cursor.next)
//      obj.map(convertEodBarRecord(_))
//    }
//
//    def queryEodBarPriorTo(time: DateTime, securityId: SecurityId): Option[Bar] = {
//      val eodBars = mongoClient("tradesim")("eod_bars")
//      val query: DBObject = MongoDBObject.empty ++ ("s" -> symbol) ++ ("te" $lt timestamp(time))
//      val orderByClause = MongoDBObject.empty ++ ("te" -> -1)
//      val cursor = (eodBars.find(query) $orderby orderByClause).limit(1)
//      val obj = Option(cursor.next)
//      obj.map(convertEodBarRecord(_))
//    }
//
//    def queryEodBars(securityId: SecurityId): Seq[Bar] = {
//      val eodBars = mongoClient("tradesim")("eod_bars")
//      val query: DBObject = MongoDBObject.empty ++ ("s" -> symbol)
//      val orderByClause = MongoDBObject.empty ++ ("ts" -> 1)
//      val cursor = (eodBars.find(query) $orderby orderByClause)
//      cursor.map(convertEodBarRecord(_)).toList
//    }
//
//    def queryEodBars(securityId: SecurityId, earliestTime: DateTime, latestTime: DateTime): Seq[Bar] = {
//      val eodBars = mongoClient("tradesim")("eod_bars")
//      val query: DBObject = MongoDBObject.empty ++ ("s" -> symbol) ++ ("ts" $gte timestamp(earliestTime)) ++ ("te" $lte timestamp(latestTime))
//      val orderByClause = MongoDBObject.empty ++("ts" -> 1)
//      val cursor = (eodBars.find(query) $orderby orderByClause)
//      cursor.map(convertEodBarRecord(_)).toList
//    }
//
//    def findOldestEodBar(securityId: SecurityId): Option[Bar] = {
//      val eodBars = mongoClient("tradesim")("eod_bars")
//      val query: DBObject = MongoDBObject.empty ++ ("s" -> symbol)
//      val orderByClause = MongoDBObject.empty ++ ("ts" -> 1)
//      val cursor = (eodBars.find(query) $orderby orderByClause).limit(1)
//      val obj = Option(cursor.next)
//      obj.map(convertEodBarRecord(_))
//    }
//
//    def findMostRecentEodBar(securityId: SecurityId): Option[Bar] = {
//      val eodBars = mongoClient("tradesim")("eod_bars")
//      val query: DBObject = MongoDBObject.empty ++ ("s" -> symbol)
//      val orderByClause = MongoDBObject.empty ++ ("ts" -> -1)
//      val cursor = (eodBars.find(query) $orderby orderByClause).limit(1)
//      val obj = Option(cursor.next)
//      obj.map(convertEodBarRecord(_))
//    }
//
//    /**
//     * Returns a sequence of Split and CashDividend records (ordered in ascending order by ex_dt - i.e. oldest to newest) for <symbol-or-symbols> that
//     * have taken effect or have been applied at some point within the interval defined by [begin-dt, end-dt].
//     * <symbol-or-symbols> may be either a single String or a vector of Strings.
//     *
//     * Assumes that there is a mongodb collection named "corporate_actions" containing the fields:
//     *   s (ticker symbol),
//     *   ex (timestamp representing the date and time at which the split/dividend takes effect or is applied)
//     *   _type (a string of either "Split" or "CashDividend")
//     * and that there is an ascending index of the form:
//     *   index([
//     *     [:s, 1],
//     *     [:ex, 1]
//     *   ])
//     */
//    def queryCorporateActions(symbols: IndexedSeq[String]): IndexedSeq[CorporateAction] = {
//      val eodBars = mongoClient("tradesim")("corporate_actions")
//      val query: DBObject = MongoDBObject.empty ++ ("s" $in symbols)
//      val orderByClause = MongoDBObject.empty ++ ("ex" -> 1)
//      val cursor = (eodBars.find(query) $orderby orderByClause)
//      cursor.map(convertCorporateActionRecord(_)).to[Vector]
//    }
//
//    def queryCorporateActions(symbols: IndexedSeq[String], startTime: DateTime, endTime: DateTime): IndexedSeq[CorporateAction] = {
//      val eodBars = mongoClient("tradesim")("corporate_actions")
//      val query: DBObject = MongoDBObject.empty ++ ("s" $in symbols) ++ ("ex" $gte timestamp(startTime)) ++ ("te" $lte timestamp(endTime))
//      val orderByClause = MongoDBObject.empty ++ ("ex" -> 1)
//      val cursor = (eodBars.find(query) $orderby orderByClause)
//      cursor.map(convertCorporateActionRecord(_)).to[Vector]
//    }
//
//    def queryQuarterlyReport(time: DateTime, securityId: SecurityId): Option[QuarterlyReport] = {
//      val quarterlyReports = mongoClient("tradesim")("quarterly_reports")
//      val query: DBObject = MongoDBObject.empty ++ ("s" -> symbol) ++ ("ts" $lte timestamp(time))
//      val orderByClause = MongoDBObject.empty ++ ("ts" -> -1)
//      val cursor = (quarterlyReports.find(query) $orderby orderByClause).limit(1)
//      val obj = Option(cursor.next)
//      obj.map(convertQuarterlyReportRecord(_))
//    }
//
//    def queryQuarterlyReportPriorTo(time: DateTime, securityId: SecurityId): Option[QuarterlyReport] = {
//
//    }
//
//    def queryQuarterlyReports(securityId: SecurityId): Seq[QuarterlyReport] = {
//
//    }
//
//    def queryQuarterlyReports(securityId: SecurityId, earliestTime: DateTime, latestTime: DateTime): Seq[QuarterlyReport] = {
//
//    }
//  }

  object SlickAdapter {
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
      def forInsert = strategyName ~ securityIds ~ principal ~ commissionPerTrade ~ commissionPerShare ~ startTime ~ endTime ~ transactionLog ~ portfolioValueLog returning id
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
      Database.forURL(jdbcConnectionString, username, password, driver = driver) withSession { session =>
        val adapter = new SlickAdapter()(session)
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


    implicit val getCorporateActionRecord = GetResult(a => (a.nextInt, a.nextString, a.nextString, a.nextInt, a.nextInt, a.nextInt, a.nextInt, a.nextBigDecimal))

    def queryCorporateActions(securityIds: IndexedSeq[Int]): IndexedSeq[CorporateAction] = {
      val sql =
      s"""
        |select * from corporate_actions
        |where security_id in (${securityIds.mkString("'", "','", "'")})
        |order by ex_date
      """.stripMargin
      Q.queryNA[CorporateActionRecord](sql).mapResult(convertCorporateActionRecord(_)).to[Vector]
    }

    def queryCorporateActions(securityIds: IndexedSeq[Int], startTime: DateTime, endTime: DateTime): IndexedSeq[CorporateAction] = {
      val sql =
        s"""
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

    def insertTrials(strategyName: String, trialStatePairs: Seq[(Trial, State)]): Seq[Int] = {
      val records = trialStatePairs.par.map(pair => buildInsertionTuple(strategyName, pair._1, pair._2)).seq
      Trials.forInsert.insertAll(records:_*)
    }

    def buildInsertionTuple(strategyName: String, trial: Trial, state: State): NewTrialRecord = {
      val symbolList = serializeThriftObject(convertSymbolListToThrift(trial.symbols))
      val principal = trial.principal.toString
      val commissionPerTrade = trial.commissionPerTrade.toString
      val commissionPerShare = trial.commissionPerShare.toString
      val startTime = timestamp(trial.startTime)
      val endTime = timestamp(trial.endTime)
      val transactionLog = serializeThriftObject(convertTransactionLogToThrift(state.transactions))
      val portfolioValueLog = serializeThriftObject(convertPortfolioValueLogToThrift(state.portfolioValueHistory))

      (strategyName, symbolList, principal, commissionPerTrade, commissionPerShare, startTime, endTime, transactionLog, portfolioValueLog)
    }

    def convertSymbolListToThrift(symbols: Seq[String]): thrift.SymbolList = {
      val list = new thrift.SymbolList()
      list.setSymbols(symbols)
      list
    }

    def convertTransactionLogToThrift(transactions: TransactionLog): thrift.TransactionLog = {
      val transactionLog = new thrift.TransactionLog()
      transactionLog.setTransactions(transactions.map(convertTransactionToThrift))
      transactionLog
    }

    def convertTransactionToThrift(transaction: Transaction): thrift.Transaction = {
      val thriftTransaction = new thrift.Transaction()
      transaction match {
        case MarketBuy(time, securityId, qty, fillPrice) =>
          thriftTransaction.setMarketBuyOrder(buildThriftMarketBuyOrder(time, securityId, qty, fillPrice))
        case MarketSell(time, securityId, qty, fillPrice) =>
          thriftTransaction.setMarketSellOrder(buildThriftMarketSellOrder(time, securityId, qty, fillPrice))
        case LimitBuy(time, securityId, qty, limitPrice, fillPrice) =>
          thriftTransaction.setLimitBuyOrder(buildThriftLimitBuyOrder(time, securityId, qty, limitPrice, fillPrice))
        case LimitSell(time, securityId, qty, limitPrice, fillPrice) =>
          thriftTransaction.setLimitSellOrder(buildThriftLimitSellOrder(time, securityId, qty, limitPrice, fillPrice))
        case CashDividendPayment(securityId, exDate, payableDate, amountPerShare, adjustmentTime, shareQty, total) =>
          thriftTransaction.setCashDividendPayment(buildThriftCashDividendPayment(securityId, exDate, payableDate, amountPerShare, adjustmentTime, shareQty, total))
        case SplitAdjustment(securityId, exDate, ratio, adjustmentTime, shareQtyDelta, cashPayout) =>
          thriftTransaction.setSplitAdjustment(buildThriftSplitAdjustment(securityId, exDate, ratio, adjustmentTime, shareQtyDelta, cashPayout))
      }
      thriftTransaction
    }

    def buildThriftMarketBuyOrder(time: DateTime, securityId: SecurityId, qty: Long, fillPrice: Option[BigDecimal]): thrift.MarketBuyOrder = {
      val order = new thrift.MarketBuyOrder(timestamp(time), symbol, qty)
      fillPrice match {
        case Some(price) => order.setFillPrice(price.toString)
        case None => order
      }
    }

    def buildThriftMarketSellOrder(time: DateTime, securityId: SecurityId, qty: Long, fillPrice: Option[BigDecimal]): thrift.MarketSellOrder = {
      val order = new thrift.MarketSellOrder(timestamp(time), symbol, qty)
      fillPrice match {
        case Some(price) => order.setFillPrice(price.toString)
        case None => order
      }
    }

    def buildThriftLimitBuyOrder(time: DateTime, securityId: SecurityId, qty: Long, limitPrice: BigDecimal, fillPrice: Option[BigDecimal]): thrift.LimitBuyOrder = {
      val order = new thrift.LimitBuyOrder(timestamp(time), symbol, qty, limitPrice.toString)
      fillPrice match {
        case Some(price) => order.setFillPrice(price.toString)
        case None => order
      }
    }

    def buildThriftLimitSellOrder(time: DateTime, securityId: SecurityId, qty: Long, limitPrice: BigDecimal, fillPrice: Option[BigDecimal]): thrift.LimitSellOrder = {
      val order = new thrift.LimitSellOrder(timestamp(time), symbol, qty, limitPrice.toString)
      fillPrice match {
        case Some(price) => order.setFillPrice(price.toString)
        case None => order
      }
    }

    def buildThriftCashDividendPayment(securityId: SecurityId,
                                       exDate: DateTime,
                                       payableDate: Option[DateTime],
                                       amountPerShare: BigDecimal,
                                       adjustmentTime: DateTime,
                                       shareQty: Long,
                                       total: BigDecimal): thrift.CashDividendPayment = {
      val dividendPayment = new thrift.CashDividendPayment(symbol,
                                                           timestamp(exDate),
                                                           amountPerShare.toString,
                                                           timestamp(adjustmentTime),
                                                           shareQty,
                                                           total.toString)
      payableDate match {
        case Some(date) => dividendPayment.setPayableDate(timestamp(date))
        case None => dividendPayment
      }
    }

    def buildThriftSplitAdjustment(securityId: SecurityId,
                                   exDate: DateTime,
                                   ratio: BigDecimal,
                                   adjustmentTime: DateTime,
                                   shareQtyDelta: Long,
                                   cashPayout: BigDecimal): thrift.SplitAdjustment = {
      new thrift.SplitAdjustment(symbol, timestamp(exDate), ratio.toString, timestamp(adjustmentTime), shareQtyDelta, cashPayout.toString)
    }

    def convertPortfolioValueLogToThrift(portfolioValueLog: Seq[PortfolioValue]): thrift.PortfolioValueHistory = {
      val portfolioValueHistory = new thrift.PortfolioValueHistory()
      portfolioValueHistory.setPortfolioValues(portfolioValueLog.map(convertPortfolioValueToThrift))
      portfolioValueHistory
    }

    def convertPortfolioValueToThrift(portfolioValue: PortfolioValue): thrift.PortfolioValue = {
      new thrift.PortfolioValue(timestamp(portfolioValue.time), portfolioValue.value.toString)
    }

    def serializeThriftObject[T <: org.apache.thrift.TBase[_, _], F <: org.apache.thrift.TFieldIdEnum](thriftObject: TBase[T, F]): String = {
      val serializer = new TSerializer(new TBinaryProtocol.Factory())
      serializer.toString(thriftObject)
    }
  }
}