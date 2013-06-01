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
import dke.tradesim.core.CashDividend
import dke.tradesim.core.EodBar
import dke.tradesim.core.AnnualReport
import dke.tradesim.core.QuarterlyReport
import dke.tradesim.core.Split
import dke.tradesim.json.LiftJValueWithFilter

object db {
  object Adapter {
    val dyn = new DynamicVariable[Adapter](null)

    /**
     * Run the supplied thunk with a new adapter and automatically close the session at the end.
     * The session is stored in a thread-local variable which can be accessed with the implicit
     * function in Database.Implicit.
     */
    def withAdapter[T](adapter: Adapter, f: => T): T = dyn.withValue(adapter)(f)

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

    def queryEodBar(time: DateTime, symbol: String): Option[Bar]
    def queryEodBarPriorTo(time: DateTime, symbol: String): Option[Bar]
    def queryEodBars(symbol: String): Seq[Bar]
    def queryEodBars(symbol: String, earliestTime: DateTime, latestTime: DateTime): Seq[Bar]
    def findOldestEodBar(symbol: String): Option[Bar]
    def findMostRecentEodBar(symbol: String): Option[Bar]

    def queryCorporateActions(symbols: IndexedSeq[String]): IndexedSeq[CorporateAction]
    def queryCorporateActions(symbols: IndexedSeq[String], startTime: DateTime, endTime: DateTime): IndexedSeq[CorporateAction]

    def queryQuarterlyReport(time: DateTime, symbol: String): Option[QuarterlyReport]
    def queryQuarterlyReportPriorTo(time: DateTime, symbol: String): Option[QuarterlyReport]
    def queryQuarterlyReports(symbol: String): Seq[QuarterlyReport]
    def queryQuarterlyReports(symbol: String, earliestTime: DateTime, latestTime: DateTime): Seq[QuarterlyReport]

    def queryAnnualReport(time: DateTime, symbol: String): Option[AnnualReport]
    def queryAnnualReportPriorTo(time: DateTime, symbol: String): Option[AnnualReport]
    def queryAnnualReports(symbol: String): Seq[AnnualReport]
    def queryAnnualReports(symbol: String, earliestTime: DateTime, latestTime: DateTime): Seq[AnnualReport]
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
//    def queryEodBar(time: DateTime, symbol: String): Option[Bar] = {
//      val eodBars = mongoClient("tradesim")("eod_bars")
//      val query: DBObject = MongoDBObject.empty ++ ("s" -> symbol) ++ ("ts" $lte timestamp(time))
//      val orderByClause = MongoDBObject.empty ++ ("ts" -> -1)
//      val cursor = (eodBars.find(query) $orderby orderByClause).limit(1)
//      val obj = Option(cursor.next)
//      obj.map(convertEodBarRecord(_))
//    }
//
//    def queryEodBarPriorTo(time: DateTime, symbol: String): Option[Bar] = {
//      val eodBars = mongoClient("tradesim")("eod_bars")
//      val query: DBObject = MongoDBObject.empty ++ ("s" -> symbol) ++ ("te" $lt timestamp(time))
//      val orderByClause = MongoDBObject.empty ++ ("te" -> -1)
//      val cursor = (eodBars.find(query) $orderby orderByClause).limit(1)
//      val obj = Option(cursor.next)
//      obj.map(convertEodBarRecord(_))
//    }
//
//    def queryEodBars(symbol: String): Seq[Bar] = {
//      val eodBars = mongoClient("tradesim")("eod_bars")
//      val query: DBObject = MongoDBObject.empty ++ ("s" -> symbol)
//      val orderByClause = MongoDBObject.empty ++ ("ts" -> 1)
//      val cursor = (eodBars.find(query) $orderby orderByClause)
//      cursor.map(convertEodBarRecord(_)).toList
//    }
//
//    def queryEodBars(symbol: String, earliestTime: DateTime, latestTime: DateTime): Seq[Bar] = {
//      val eodBars = mongoClient("tradesim")("eod_bars")
//      val query: DBObject = MongoDBObject.empty ++ ("s" -> symbol) ++ ("ts" $gte timestamp(earliestTime)) ++ ("te" $lte timestamp(latestTime))
//      val orderByClause = MongoDBObject.empty ++("ts" -> 1)
//      val cursor = (eodBars.find(query) $orderby orderByClause)
//      cursor.map(convertEodBarRecord(_)).toList
//    }
//
//    def findOldestEodBar(symbol: String): Option[Bar] = {
//      val eodBars = mongoClient("tradesim")("eod_bars")
//      val query: DBObject = MongoDBObject.empty ++ ("s" -> symbol)
//      val orderByClause = MongoDBObject.empty ++ ("ts" -> 1)
//      val cursor = (eodBars.find(query) $orderby orderByClause).limit(1)
//      val obj = Option(cursor.next)
//      obj.map(convertEodBarRecord(_))
//    }
//
//    def findMostRecentEodBar(symbol: String): Option[Bar] = {
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
//    def queryQuarterlyReport(time: DateTime, symbol: String): Option[QuarterlyReport] = {
//      val quarterlyReports = mongoClient("tradesim")("quarterly_reports")
//      val query: DBObject = MongoDBObject.empty ++ ("s" -> symbol) ++ ("ts" $lte timestamp(time))
//      val orderByClause = MongoDBObject.empty ++ ("ts" -> -1)
//      val cursor = (quarterlyReports.find(query) $orderby orderByClause).limit(1)
//      val obj = Option(cursor.next)
//      obj.map(convertQuarterlyReportRecord(_))
//    }
//
//    def queryQuarterlyReportPriorTo(time: DateTime, symbol: String): Option[QuarterlyReport] = {
//
//    }
//
//    def queryQuarterlyReports(symbol: String): Seq[QuarterlyReport] = {
//
//    }
//
//    def queryQuarterlyReports(symbol: String, earliestTime: DateTime, latestTime: DateTime): Seq[QuarterlyReport] = {
//
//    }
//  }

  object SlickAdapter {
    type EodBarRecord = (Int, String, Timestamp, Long, String, String, String, String, Long)
    object EodBars extends Table[EodBarRecord]("eod_bars") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)   // This is the primary key column
      def symbol = column[String]("symbol")
      def startTime = column[Timestamp]("start_time")
      def endTime = column[Timestamp]("end_time")
      def open = column[String]("open")
      def high = column[String]("high")
      def low = column[String]("low")
      def close = column[String]("close")
      def volume = column[Long]("volume")

      // Every table needs a * projection with the same type as the table's type parameter
      def * = id ~ symbol ~ startTime ~ endTime ~ open ~ high ~ low ~ close ~ volume
    }

    def convertEodBarRecord(record: EodBarRecord): EodBar = {
      record match {
        case (_, symbol, startTime, endTime, open, high, low, close, volume) =>
          EodBar(symbol, datetime(startTime), datetime(endTime), BigDecimal(open), BigDecimal(high), BigDecimal(low), BigDecimal(close), volume)
      }
    }

    def convertEodBarRecord(record: Option[EodBarRecord]): Option[EodBar] = record.map(convertEodBarRecord(_))


    type CorporateActionRecord = (Int, String, String, Datestamp, Datestamp, Datestamp, Datestamp, String)
    object CorporateActions extends Table[CorporateActionRecord]("corporate_actions") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)   // This is the primary key column
      def kind = column[String]("type")                     // either Split or CashDividend
      def symbol = column[String]("symbol")

      def declarationDate = column[Datestamp]("declaration_date")
      def exDate = column[Datestamp]("ex_date")
      def recordDate = column[Datestamp]("record_date")
      def payableDate = column[Datestamp]("payable_date")

      def ratioOrAmount = column[String]("number")      // split ratio OR dividend amount

      // Every table needs a * projection with the same type as the table's type parameter
      def * = id ~ kind ~ symbol ~ declarationDate ~ exDate ~ recordDate ~ payableDate ~ ratioOrAmount
    }

    def convertCorporateActionRecord(record: CorporateActionRecord): CorporateAction = {
      record match {
        case (_, "Split", symbol, _, exDate, _, _, ratio) =>
          Split(symbol, datetime(exDate), BigDecimal(ratio))
        case (_, "CashDividend", symbol, declarationDate, exDate, recordDate, payableDate, amount) =>
          CashDividend(symbol, datetime(declarationDate), datetime(exDate), datetime(recordDate), datetime(payableDate), BigDecimal(amount))
      }
    }


    type QuarterlyReportRecord = (Int, String, Timestamp, Timestamp, Timestamp, String, String, String)
    object QuarterlyReports extends Table[QuarterlyReportRecord]("quarterly_reports") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)   // This is the primary key column
      def symbol = column[String]("symbol")
      def startTime = column[Timestamp]("start_time")
      def endTime = column[Timestamp]("end_time")
      def publicationTime = column[Timestamp]("publication_time")
      def incomeStatement = column[String]("income_statement")
      def balanceSheet = column[String]("balance_sheet")
      def cashFlowStatement = column[String]("cash_flow_statement")

      // Every table needs a * projection with the same type as the table's type parameter
      def * = id ~ symbol ~ startTime ~ endTime ~ publicationTime ~ incomeStatement ~ balanceSheet ~ cashFlowStatement
    }

    type AnnualReportRecord = (Int, String, Timestamp, Timestamp, Timestamp, String, String, String)
    object AnnualReports extends Table[AnnualReportRecord]("annual_reports") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)   // This is the primary key column
      def symbol = column[String]("symbol")
      def startTime = column[Timestamp]("start_time")
      def endTime = column[Timestamp]("end_time")
      def publicationTime = column[Timestamp]("publication_time")
      def incomeStatement = column[String]("income_statement")
      def balanceSheet = column[String]("balance_sheet")
      def cashFlowStatement = column[String]("cash_flow_statement")

      // Every table needs a * projection with the same type as the table's type parameter
      def * = id ~ symbol ~ startTime ~ endTime ~ publicationTime ~ incomeStatement ~ balanceSheet ~ cashFlowStatement
    }

    // assumes json is of the form: [ "header", [attribute, value], [attribute, value], "header", [attr, val], ... ]
    // where the json structure is no more than 1 level deep (i.e. there are no sub-sections)
    def convertJsonToStatement(jsonString: String): Statement = {
      for {
        JArray(lineItems) <- parse(jsonString, useBigDecimalForDouble = true)
        lineItem <- lineItems
      } yield lineItem match {
        case JString(text) => HeaderLineItem(text)
        case JArray(pairValues) => pairValues(1) match {
          case JString(value) => StringLineItem(getString(pairValues(0)).getOrElse(""), value)
          case JDecimal(value) => NumericLineItem(getString(pairValues(0)).getOrElse(""), value)
          case _ => throw new Exception(s"Unable to parse statement. It is malformed: $jsonString")
        }
        case _ => throw new Exception(s"Unable to parse statement. It is malformed: $jsonString")
      }
    }

    def getString(jValue: JValue): Option[String] = jValue match {
      case JString(str) => Option(str)
      case _ => None
    }

    def convertQuarterlyReportRecord(record: QuarterlyReportRecord): QuarterlyReport = {
      record match {
        case (_, symbol, startTime, endTime, publicationTime, incomeStatement, balanceSheet, cashFlowStatement) =>
          QuarterlyReport(
            symbol,
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
        case (_, symbol, startTime, endTime, publicationTime, incomeStatement, balanceSheet, cashFlowStatement) =>
          AnnualReport(
            symbol,
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


    def withAdapter[T](jdbcConnectionString: String, driver: String)(f: => T): T = {
      Database.forURL(jdbcConnectionString, driver = driver) withSession { session =>
        val adapter = new SlickAdapter()(session)
        Adapter.withAdapter(adapter, f)
      }
    }
  }

  class SlickAdapter(implicit val session: Session) extends Adapter {
    import SlickAdapter._

    def createDb() {
      val ddl: scala.slick.lifted.DDL = EodBars.ddl ++ CorporateActions.ddl ++ QuarterlyReports.ddl ++ AnnualReports.ddl
      ddl.create
    }

    def queryEodBar(time: DateTime, symbol: String): Option[Bar] = {
      val bars = Query(EodBars).filter(_.symbol === symbol).filter(_.startTime <= timestamp(time))
      val sortedBars = bars.sortBy(_.startTime.desc)
      val record = sortedBars.take(1).firstOption
      convertEodBarRecord(record)
    }

    def queryEodBarPriorTo(time: DateTime, symbol: String): Option[Bar] = {
      val bars = Query(EodBars).filter(_.symbol === symbol).filter(_.endTime < timestamp(time))
      val sortedBars = bars.sortBy(_.endTime.desc)
      val record = sortedBars.take(1).firstOption
      convertEodBarRecord(record)
    }

    def queryEodBars(symbol: String): Seq[Bar] = {
      val bars = Query(EodBars).filter(_.symbol === symbol)
      val sortedBars = bars.sortBy(_.startTime)
      sortedBars.mapResult(convertEodBarRecord(_)).list
    }

    def queryEodBars(symbol: String, earliestTime: DateTime, latestTime: DateTime): Seq[Bar] = {
      val bars = Query(EodBars).filter(_.symbol === symbol)
                               .filter(_.startTime >= timestamp(earliestTime))
                               .filter(_.endTime <= timestamp(latestTime))
      val sortedBars = bars.sortBy(_.startTime)
      sortedBars.mapResult(convertEodBarRecord(_)).list
    }

    def findOldestEodBar(symbol: String): Option[Bar] = {
      val bars = Query(EodBars).filter(_.symbol === symbol)
      val sortedBars = bars.sortBy(_.startTime)
      val record = sortedBars.take(1).firstOption
      convertEodBarRecord(record)
    }

    def findMostRecentEodBar(symbol: String): Option[Bar] = {
      val bars = Query(EodBars).filter(_.symbol === symbol)
      val sortedBars = bars.sortBy(_.startTime.desc)
      val record = sortedBars.take(1).firstOption
      convertEodBarRecord(record)
    }


    implicit val getCorporateActionRecord = GetResult(a => (a.nextInt, a.nextString, a.nextString, a.nextInt, a.nextInt, a.nextInt, a.nextInt, a.nextBigDecimal))

    def queryCorporateActions(symbols: IndexedSeq[String]): IndexedSeq[CorporateAction]= {
      val sql =
      s"""
        |select * from corporate_actions
        |where symbol in (${symbols.mkString("'", "','", "'")})
        |order by ex_date
      """.stripMargin
      Q.queryNA[CorporateActionRecord](sql).mapResult(convertCorporateActionRecord(_)).to[Vector]
    }

    def queryCorporateActions(symbols: IndexedSeq[String], startTime: DateTime, endTime: DateTime): IndexedSeq[CorporateAction] = {
      val sql =
        s"""
        |select * from corporate_actions
        |where symbol in (${symbols.mkString("'", "','", "'")})
        |and ex_date >= ${timestamp(startTime)}
        |and ex_date <= ${timestamp(endTime)}
        |order by ex_date
      """.stripMargin
      Q.queryNA[CorporateActionRecord](sql).mapResult(convertCorporateActionRecord(_)).to[Vector]
    }


    def queryQuarterlyReport(time: DateTime, symbol: String): Option[QuarterlyReport] = {
      val reports = Query(QuarterlyReports).filter(_.symbol === symbol).filter(_.startTime <= timestamp(time))
      val sortedReports = reports.sortBy(_.startTime.desc)
      val record = sortedReports.take(1).firstOption
      convertQuarterlyReportRecord(record)
    }

    def queryQuarterlyReportPriorTo(time: DateTime, symbol: String): Option[QuarterlyReport] = {
      val reports = Query(QuarterlyReports).filter(_.symbol === symbol).filter(_.endTime < timestamp(time))
      val sortedReports = reports.sortBy(_.endTime.desc)
      val record = sortedReports.take(1).firstOption
      convertQuarterlyReportRecord(record)
    }

    def queryQuarterlyReports(symbol: String): Seq[QuarterlyReport] = {
      val reports = Query(QuarterlyReports).filter(_.symbol === symbol)
      val sortedReports = reports.sortBy(_.startTime)
      sortedReports.mapResult(convertQuarterlyReportRecord(_)).list
    }

    def queryQuarterlyReports(symbol: String, earliestTime: DateTime, latestTime: DateTime): Seq[QuarterlyReport] = {
      val reports = Query(QuarterlyReports).filter(_.symbol === symbol)
                                           .filter(_.startTime >= timestamp(earliestTime))
                                           .filter(_.endTime <= timestamp(latestTime))
      val sortedReports = reports.sortBy(_.startTime)
      sortedReports.mapResult(convertQuarterlyReportRecord(_)).list
    }


    def queryAnnualReport(time: DateTime, symbol: String): Option[AnnualReport] = {
      val reports = Query(AnnualReports).filter(_.symbol === symbol).filter(_.startTime <= timestamp(time))
      val sortedReports = reports.sortBy(_.startTime.desc)
      val record = sortedReports.take(1).firstOption
      convertAnnualReportRecord(record)
    }

    def queryAnnualReportPriorTo(time: DateTime, symbol: String): Option[AnnualReport] = {
      val reports = Query(AnnualReports).filter(_.symbol === symbol).filter(_.endTime < timestamp(time))
      val sortedReports = reports.sortBy(_.endTime.desc)
      val record = sortedReports.take(1).firstOption
      convertAnnualReportRecord(record)
    }

    def queryAnnualReports(symbol: String): Seq[AnnualReport] = {
      val reports = Query(AnnualReports).filter(_.symbol === symbol)
      val sortedReports = reports.sortBy(_.startTime)
      sortedReports.mapResult(convertAnnualReportRecord(_)).list
    }

    def queryAnnualReports(symbol: String, earliestTime: DateTime, latestTime: DateTime): Seq[AnnualReport] = {
      val reports = Query(AnnualReports).filter(_.symbol === symbol)
                                        .filter(_.startTime >= timestamp(earliestTime))
                                        .filter(_.endTime <= timestamp(latestTime))
      val sortedReports = reports.sortBy(_.startTime)
      sortedReports.mapResult(convertAnnualReportRecord(_)).list
    }
  }
}