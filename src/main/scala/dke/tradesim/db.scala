package dke.tradesim

import com.mongodb.casbah.Imports._

import scala.slick.driver.PostgresDriver.simple._
import scala.slick.lifted.Query
import scala.slick.session.Database
import scala.slick.jdbc.{GetResult, StaticQuery => Q}

import dke.tradesim.core.{Bar, EodBar}
import dke.tradesim.datetimeUtils.{timestamp, datetime, Datestamp, Timestamp}
import dke.tradesim.splitsDividends.{CashDividend, Split, CorporateAction}
import org.joda.time.DateTime
import scala.util.DynamicVariable

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
    def queryEodBar(time: DateTime, symbol: String): Option[Bar]
    def queryEodBarPriorTo(time: DateTime, symbol: String): Option[Bar]
    def queryEodBars(symbol: String): Seq[Bar]
    def queryEodBars(symbol: String, earliestTime: DateTime, latestTime: DateTime): Seq[Bar]
    def findOldestEodBar(symbol: String): Option[Bar]
    def findMostRecentEodBar(symbol: String): Option[Bar]

    def queryCorporateActions(symbols: IndexedSeq[String]): IndexedSeq[CorporateAction]
    def queryCorporateActions(symbols: IndexedSeq[String], startTime: DateTime, endTime: DateTime): IndexedSeq[CorporateAction]
  }

  object MongoAdapter {
    def withAdapter[T](mongoDbConnectionString: String)(f: => T): T = {
      val uri = new MongoClientURI(mongoDbConnectionString)
      val mongoClient = MongoClient(uri)
      val adapter = new MongoAdapter(mongoClient)
      Adapter.withAdapter(adapter, f)
    }

    def convertEodBarRecord(record: MongoDBObject): EodBar = {
      EodBar(
        record.as[String]("s"),
        datetime(record.as[Long]("ts")),
        datetime(record.as[Long]("te")),
        record.as[BigDecimal]("o"),
        record.as[BigDecimal]("h"),
        record.as[BigDecimal]("l"),
        record.as[BigDecimal]("c"),
        record.as[Long]("v")
      )
    }

    def convertCorporateActionRecord(record: MongoDBObject): CorporateAction = {
      val kind = record.as[String]("_type")
      kind match {
        case "Split" => convertSplitRecord(record)
        case "CashDividend" => convertCashDividendRecord(record)
        case _ => throw new IllegalArgumentException("A corporate action's _type must be either Split or CashDividend")
      }
    }

    def convertSplitRecord(record: MongoDBObject): Split = {
      Split(
        record.as[String]("s"),
        datetime(record.as[Long]("ex")),
        record.as[BigDecimal]("r")
      )
    }

    def convertCashDividendRecord(record: MongoDBObject): CashDividend = {
      CashDividend(
        record.as[String]("s"),
        datetime(record.as[Long]("decl")),
        datetime(record.as[Long]("ex")),
        datetime(record.as[Long]("rec")),
        datetime(record.as[Long]("pay")),
        record.as[BigDecimal]("a")
      )
    }

  }

  class MongoAdapter(val mongoClient: MongoClient) extends Adapter {
    import MongoAdapter._

    def queryEodBar(time: DateTime, symbol: String): Option[Bar] = {
      val eodBars = mongoClient("tradesim")("eod_bars")
      val query: DBObject = MongoDBObject.empty ++ ("s" -> symbol) ++ ("ts" $lte timestamp(time))
      val orderByClause = MongoDBObject.empty ++ ("ts" -> -1)
      val cursor = (eodBars.find(query) $orderby orderByClause).limit(1)
      val obj = Option(cursor.next)
      obj.map(convertEodBarRecord(_))
    }

    def queryEodBarPriorTo(time: DateTime, symbol: String): Option[Bar] = {
      val eodBars = mongoClient("tradesim")("eod_bars")
      val query: DBObject = MongoDBObject.empty ++ ("s" -> symbol) ++ ("te" $lt timestamp(time))
      val orderByClause = MongoDBObject.empty ++ ("te" -> -1)
      val cursor = (eodBars.find(query) $orderby orderByClause).limit(1)
      val obj = Option(cursor.next)
      obj.map(convertEodBarRecord(_))
    }

    def queryEodBars(symbol: String): Seq[Bar] = {
      val eodBars = mongoClient("tradesim")("eod_bars")
      val query: DBObject = MongoDBObject.empty ++ ("s" -> symbol)
      val orderByClause = MongoDBObject.empty ++ ("ts" -> 1)
      val cursor = (eodBars.find(query) $orderby orderByClause)
      cursor.map(convertEodBarRecord(_)).toList
    }

    def queryEodBars(symbol: String, earliestTime: DateTime, latestTime: DateTime): Seq[Bar] = {
      val eodBars = mongoClient("tradesim")("eod_bars")
      val query: DBObject = MongoDBObject.empty ++ ("s" -> symbol) ++ ("ts" $gte timestamp(earliestTime)) ++ ("te" $lte timestamp(latestTime))
      val orderByClause = MongoDBObject.empty ++("ts" -> 1)
      val cursor = (eodBars.find(query) $orderby orderByClause)
      cursor.map(convertEodBarRecord(_)).toList
    }

    def findOldestEodBar(symbol: String): Option[Bar] = {
      val eodBars = mongoClient("tradesim")("eod_bars")
      val query: DBObject = MongoDBObject.empty ++ ("s" -> symbol)
      val orderByClause = MongoDBObject.empty ++ ("ts" -> 1)
      val cursor = (eodBars.find(query) $orderby orderByClause).limit(1)
      val obj = Option(cursor.next)
      obj.map(convertEodBarRecord(_))
    }

    def findMostRecentEodBar(symbol: String): Option[Bar] = {
      val eodBars = mongoClient("tradesim")("eod_bars")
      val query: DBObject = MongoDBObject.empty ++ ("s" -> symbol)
      val orderByClause = MongoDBObject.empty ++ ("ts" -> -1)
      val cursor = (eodBars.find(query) $orderby orderByClause).limit(1)
      val obj = Option(cursor.next)
      obj.map(convertEodBarRecord(_))
    }

    /**
     * Returns a sequence of Split and CashDividend records (ordered in ascending order by ex_dt - i.e. oldest to newest) for <symbol-or-symbols> that
     * have taken effect or have been applied at some point within the interval defined by [begin-dt, end-dt].
     * <symbol-or-symbols> may be either a single String or a vector of Strings.
     *
     * Assumes that there is a mongodb collection named "corporate_actions" containing the fields:
     *   s (ticker symbol),
     *   ex (timestamp representing the date and time at which the split/dividend takes effect or is applied)
     *   _type (a string of either "Split" or "CashDividend")
     * and that there is an ascending index of the form:
     *   index([
     *     [:s, 1],
     *     [:ex, 1]
     *   ])
     */
    def queryCorporateActions(symbols: IndexedSeq[String]): IndexedSeq[CorporateAction] = {
      val eodBars = mongoClient("tradesim")("corporate_actions")
      val query: DBObject = MongoDBObject.empty ++ ("s" $in symbols)
      val orderByClause = MongoDBObject.empty ++ ("ex" -> 1)
      val cursor = (eodBars.find(query) $orderby orderByClause)
      cursor.map(convertCorporateActionRecord(_)).to[Vector]
    }

    def queryCorporateActions(symbols: IndexedSeq[String], startTime: DateTime, endTime: DateTime): IndexedSeq[CorporateAction] = {
      val eodBars = mongoClient("tradesim")("corporate_actions")
      val query: DBObject = MongoDBObject.empty ++ ("s" $in symbols) ++ ("ex" $gte timestamp(startTime)) ++ ("te" $lte timestamp(endTime))
      val orderByClause = MongoDBObject.empty ++ ("ex" -> 1)
      val cursor = (eodBars.find(query) $orderby orderByClause)
      cursor.map(convertCorporateActionRecord(_)).to[Vector]
    }
  }

  object SlickAdapter {
    type EodBarRecord = (Int, String, Timestamp, Long, BigDecimal, BigDecimal, BigDecimal, BigDecimal, Long)
    object EodBars extends Table[EodBarRecord]("eod_bars") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)   // This is the primary key column
      def symbol = column[String]("symbol")
      def startTime = column[Timestamp]("start_time")
      def endTime = column[Timestamp]("end_time")
      def open = column[BigDecimal]("open")
      def high = column[BigDecimal]("high")
      def low = column[BigDecimal]("low")
      def close = column[BigDecimal]("close")
      def volume = column[Long]("volume")

      // Every table needs a * projection with the same type as the table's type parameter
      def * = id ~ symbol ~ startTime ~ endTime ~ open ~ high ~ low ~ close ~ volume
    }

    def convertEodBarRecord(record: EodBarRecord): EodBar = {
      record match {
        case (_, symbol, startTime, endTime, open, high, low, close, volume) =>
          EodBar(symbol, datetime(startTime), datetime(endTime), open, high, low, close, volume)
      }
    }

    def convertEodBarRecord(record: Option[EodBarRecord]): Option[EodBar] = record.map(convertEodBarRecord(_))


    type CorporateActionRecord = (Int, String, String, Datestamp, Datestamp, Datestamp, Datestamp, BigDecimal)
    //  object CorporateActions extends Table[CorporateActionRecord]("corporate_actions") {
    //    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)   // This is the primary key column
    //    def kind = column[String]("type")                     // either Split or CashDividend
    //    def symbol = column[String]("symbol")
    //
    //    def declarationDate = column[Datestamp]("declaration_date")
    //    def exDate = column[Datestamp]("ex_date")
    //    def recordDate = column[Datestamp]("record_date")
    //    def payableDate = column[Datestamp]("payable_date")
    //
    //    def ratioOrAmount = column[BigDecimal]("number")      // split ratio OR dividend amount
    //
    //    // Every table needs a * projection with the same type as the table's type parameter
    //    def * = id ~ kind ~ symbol ~ declarationDate ~ exDate ~ recordDate ~ payableDate ~ ratioOrAmount
    //  }

    def convertCorporateActionRecord(record: CorporateActionRecord): CorporateAction = {
      record match {
        case (_, "Split", symbol, _, exDate, _, _, ratio) =>
          Split(symbol, datetime(exDate), ratio)
        case (_, "CashDividend", symbol, declarationDate, exDate, recordDate, payableDate, amount) =>
          CashDividend(symbol, datetime(declarationDate), datetime(exDate), datetime(recordDate), datetime(payableDate), amount)
      }
    }

    def withAdapter[T](jdbcConnectionString: String, driver: String)(f: => T): T = {
      Database.forURL(jdbcConnectionString, driver = driver) withSession { session =>
        val adapter = new SlickAdapter()(session)
        Adapter.withAdapter(adapter, f)
      }
    }
  }

  class SlickAdapter(implicit val session: Session) extends Adapter {
    import SlickAdapter._

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
  }
}