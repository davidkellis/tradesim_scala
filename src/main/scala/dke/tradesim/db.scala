package dke.tradesim

import scala.slick.driver.PostgresDriver.simple._

object db {
  object EodBars extends Table[(Int, String, Long, Long, BigDecimal, BigDecimal, BigDecimal, BigDecimal, Long)]("eod_bars") {
    def id = column[Int]("id", O.PrimaryKey)   // This is the primary key column
    def symbol = column[String]("symbol")
    def startTime = column[Long]("startTime")
    def endTime = column[Long]("endTime")
    def open = column[BigDecimal]("open")
    def high = column[BigDecimal]("high")
    def low = column[BigDecimal]("low")
    def close = column[BigDecimal]("close")
    def volume = column[Long]("volume")

    // Every table needs a * projection with the same type as the table's type parameter
    def * = id ~ symbol ~ startTime ~ endTime ~ open ~ high ~ low ~ close ~ volume
  }
}