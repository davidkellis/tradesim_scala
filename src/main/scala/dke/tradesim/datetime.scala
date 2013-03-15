package dke.tradesim

import org.joda.time._

object datetime {
  def timestamp(datetime: ReadableDateTime): String = datetime.toString("yyyyMMddHHmmss")
}

