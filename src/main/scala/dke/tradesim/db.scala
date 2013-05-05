package dke.tradesim

import scala.slick.driver.PostgresDriver.simple._
import dke.tradesim.core.{EodBar}
import dke.tradesim.datetimeUtils.{datetime, Datestamp, Timestamp}
import dke.tradesim.splitsDividends.{CashDividend, Split, CorporateAction}

object db {
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
}