package dke.tradesim

import org.joda.time._
import util.Random
import org.joda.time.format.{PeriodFormatter, PeriodFormatterBuilder, ISOPeriodFormat}

object datetimeUtils {
  type Timestamp = Long
  type Datestamp = Int

  val EasternTimeZone = findTimeZone("US/Eastern")
  val CentralTimeZone = findTimeZone("US/Central")
//  val PacificTimeZone = findTimeZone("US/Pacific")

  implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(isBefore(_, _))
  implicit def localDateOrdering: Ordering[LocalDate] = Ordering.fromLessThan[LocalDate](_.isBefore(_))

  def currentTime(timeZone: DateTimeZone = EasternTimeZone): DateTime = DateTime.now(timeZone)

  def date(year: Int): LocalDate = new LocalDate(year, 1, 1)
  def date(year: Int, month: Int): LocalDate = new LocalDate(year, month, 1)
  def date(year: Int, month: Int, day: Int): LocalDate = new LocalDate(year, month, day)

  def datetime(year: Int, month: Int): DateTime = datetime(year, month, 1, 0, 0, 0)
  def datetime(year: Int, month: Int, day: Int): DateTime = datetime(year, month, day, 0, 0, 0)
  def datetime(year: Int, month: Int, day: Int, hour: Int, minute: Int, second: Int): DateTime =
    new DateTime(year, month, day, hour, minute, second, EasternTimeZone)

  def datetime(datestamp: Datestamp): DateTime = {
    val ds = datestamp.toString
    val year = ds.substring(0, 4).toInt
    val month = ds.substring(4, 6).toInt
    val day = ds.substring(6, 8).toInt
    datetime(year, month, day)
  }

  def datetime(timestamp: Timestamp): DateTime = {
    val ts = timestamp.toString
    val year = ts.substring(0, 4).toInt
    val month = ts.substring(4, 6).toInt
    val day = ts.substring(6, 8).toInt
    val hour = ts.substring(8, 10).toInt
    val minute = ts.substring(10, 12).toInt
    val second = ts.substring(12, 14).toInt
    datetime(year, month, day, hour, minute, second)
  }

  def timestamp(datetime: ReadableDateTime): Timestamp = datetime.toString("yyyyMMddHHmmss").toLong

  def findTimeZone(timeZoneName: String): DateTimeZone = DateTimeZone.forID(timeZoneName)

  def midnight(datetime: DateTime): DateTime = datetime.toDateMidnight.toDateTime

  def years(n: Int): Period = Years.years(n).toPeriod
  def days(n: Int): Period = Days.days(n).toPeriod
  def hours(n: Int): Period = Hours.hours(n).toPeriod
  def millis(n: Long): Period = new Period(n)

  def periodBetween(t1: ReadableInstant, t2: ReadableInstant): Period = new Period(t1, t2)
  def periodBetween(t1: ReadablePartial, t2: ReadablePartial): Period = new Period(t1, t2)

  def durationBetween(t1: DateTime, t2: DateTime): Duration = new Duration(t1, t2)

  def intervalBetween(t1: DateTime, t2: DateTime): Interval = new Interval(t1, t2)

  def prettyFormatPeriod(period: Period): String = {
    val formatter = new PeriodFormatterBuilder()
      .appendDays()
      .appendSuffix("d")
      .appendHours()
      .appendSuffix("h")
      .appendMinutes()
      .appendSuffix("m")
      .appendSeconds()
      .appendSuffix("s")
      .appendMillis()
      .appendSuffix("ms")
      .toFormatter();
    formatter.print(period)
  }

  // t1 <= instant < t2
  def isInstantBetween(instant: DateTime, t1: DateTime, t2: DateTime): Boolean = intervalBetween(t1, t2).contains(instant)

  // t1 <= instant <= t2
  def isInstantBetweenInclusive(instant: DateTime, t1: DateTime, t2: DateTime): Boolean = !isBefore(instant, t1) && !isAfter(instant, t2)

  // Returns a random datetimeUtils between t1 (inclusive) and t2 (exclusive)
  def randomDateTime(t1: DateTime, t2: DateTime): DateTime = {
    val r = new Random().nextDouble()
    val duration = durationBetween(t1, t2)
    val millisecondOffset = (r * duration.getMillis()).toLong
    t1.plus(millisecondOffset)
  }

  def isIntervalEmpty(interval: Interval): Boolean = {
    interval.getStart() == interval.getEnd()
  }

  // t1 < t2
  def isBefore(t1: DateTime, t2: DateTime): Boolean = t1.isBefore(t2)

  // t1 > t2
  def isAfter(t1: DateTime, t2: DateTime): Boolean = t1.isAfter(t2)

  def compareDateTimes(t1: DateTime, t2: DateTime): Int = t1.compareTo(t2)
  def compareDateTimes(d1: LocalDate, d2: LocalDate): Int = d1.compareTo(d2)

  def isBeforeOrEqual(t1: DateTime, t2: DateTime): Boolean = compareDateTimes(t1, t2) <= 0
  def isBeforeOrEqual(d1: LocalDate, d2: LocalDate): Boolean = compareDateTimes(d1, d2) <= 0

  def isAfterOrEqual(t1: DateTime, t2: DateTime): Boolean = compareDateTimes(t1, t2) >= 0
  def isAfterOrEqual(d1: LocalDate, d2: LocalDate): Boolean = compareDateTimes(d1, d2) >= 0

  def maxDateTime(t1: DateTime, t2: DateTime): DateTime = if (t1.isAfter(t2)) t1 else t2

  def minDateTime(t1: DateTime, t2: DateTime): DateTime = if (t1.isBefore(t2)) t1 else t2

  // direction must be either 'before or 'after
  def offsetDateTime(t: DateTime, direction: Symbol, magnitude: Period): DateTime = direction match {
    case 'before => t.minus(magnitude)
    case 'after => t.plus(magnitude)
    case _ => t
  }

  def offsetInterval(interval: Interval,
                     startOffsetDirection: Symbol,
                     startOffsetMagnitude: Period,
                     endOffsetDirection: Symbol,
                     endOffsetMagnitude: Period): Interval = {
    val adjustedStart = offsetDateTime(interval.getStart, startOffsetDirection, startOffsetMagnitude)
    val adjustedEnd = offsetDateTime(interval.getEnd, endOffsetDirection, endOffsetMagnitude)
    intervalBetween(adjustedStart, adjustedEnd)
  }

  // returns an infinite sequence
  def timeSeries(startTime: DateTime, nextTimeFn: (DateTime) => DateTime): Stream[DateTime] = Stream.iterate(startTime)(nextTimeFn)

  // returns an infinite sequence
  def dateSeries(startDate: LocalDate, nextDateFn: (LocalDate) => LocalDate): Stream[LocalDate] = Stream.iterate(startDate)(nextDateFn)

  // returns an infinite sequence
  def interspersedTimeSeries(startTime: DateTime, period: ReadablePeriod): Stream[DateTime] = timeSeries(startTime, (t: DateTime) => t.plus(period))

  // returns an infinite sequence
  def interspersedDateSeries(startDate: LocalDate, period: ReadablePeriod): Stream[LocalDate] = dateSeries(startDate, (d: LocalDate) => d.plus(period))

  def interspersedTimeSeries(startTime: DateTime, endTime: DateTime, period: ReadablePeriod): Stream[DateTime] =
    interspersedTimeSeries(startTime, period).takeWhile(isBeforeOrEqual(_, endTime))

  def interspersedDateSeries(startDate: LocalDate, endDate: LocalDate, period: ReadablePeriod): Stream[LocalDate] =
    interspersedDateSeries(startDate, period).takeWhile(isBeforeOrEqual(_, endDate))

  // returns an infinite sequence
  def interspersedIntervals(startTime: DateTime, intervalLength: Period, separationLength: Period): Stream[Interval] = {
    val startTimes = interspersedTimeSeries(startTime, separationLength)
    startTimes.map(t => intervalBetween(t, t.plus(intervalLength)))
  }

  def interspersedIntervals(startTimeInterval: Interval, intervalLength: Period, separationLength: Period): Stream[Interval] = {
    val startTimes = interspersedTimeSeries(startTimeInterval.getStart, separationLength).takeWhile(isBeforeOrEqual(_, startTimeInterval.getEnd))
    startTimes.map(t => intervalBetween(t, t.plus(intervalLength)))
  }

  def daysInMonth(month: Int, year: Int): Int = datetime(year, month).dayOfMonth().getMaximumValue()

  def dayOfWeek(t: DateTime): Int = t.getDayOfWeek()
  def dayOfWeek(t: LocalDate): Int = t.getDayOfWeek()

  /**
   * returns the number of days that must be added to the first day of the given month to arrive at the first
   *   occurrence of the <desired-weekday> in that month; put another way, it returns the number of days
   *   that must be added to the first day of the given month to arrive at the <desired-weekday> in the first
   *   week of that month.
   * The return value will be an integer in the range [0, 6].
   * NOTE: the return value is the result of the following expression:
   *   (desired-weekday - dayOfWeek(year, month, 1) + 7) mod 7
   * desired-weekday is an integer indicating the desired day of the week, s.t. 1=Monday, 2=Tue., ..., 6=Sat., 7=Sun.
   * month is an integer indicating the month, s.t. 1=Jan., 2=Feb., ..., 11=Nov., 12=Dec.
   * year is an integer indicating the year (e.g. 1999, 2010, 2012, etc.)
   * Example:
   *   offsetOfFirstWeekdayInMonth(1, 2, 2012)    ; monday
   *   > 5
   *   offsetOfFirstWeekdayInMonth(3, 2, 2012)    ; wednesday
   *   > 0
   *   offsetOfFirstWeekdayInMonth(5, 2, 2012)    ; friday
   *   > 2
   */
  def offsetOfFirstWeekdayInMonth(desiredWeekday: Int, month: Int, year: Int): Int =
    (desiredWeekday - dayOfWeek(datetime(year, month)) + 7) % 7

  /**
   * returns a LocalDate representing the nth weekday in the given month.
   * Example:
   *   nthWeekday(3, DateTimeConstants.MONDAY, 1, 2012)   ; returns the 3rd monday in January 2012.
   *   => #<LocalDate 2012-01-16>
   *   nthWeekday(3, DateTimeConstants.MONDAY, 2, 2012)   ; returns the 3rd monday in February 2012.
   *   => #<LocalDate 2012-02-20>
   */
  def nthWeekday(n: Int, desiredWeekday: Int, month: Int, year: Int): LocalDate = {
    val firstDayOfTheMonth = date(year, month)
    val firstDesiredWeekdayOfTheMonth = firstDayOfTheMonth.plus(Days.days(offsetOfFirstWeekdayInMonth(desiredWeekday, month, year)))
    val weekOffsetInDays = Days.days(7 * (n - 1))
    firstDesiredWeekdayOfTheMonth.plus(weekOffsetInDays)
  }

  /**
   * Returns a LocalDate representing the last weekday in the given month.
   * source: http://www.irt.org/articles/js050/
   * formula:
   *   daysInMonth - (DayOfWeek(daysInMonth,month,year) - desiredWeekday + 7)%7
   * Example:
   *   lastWeekday(1, 2, 2012)     ; last monday in february 2012
   *   > #<DateTime 2012-02-27>
   */
  def lastWeekday(desiredWeekday: Int, month: Int, year: Int): LocalDate = {
    val days = daysInMonth(month, year)
    val dayOfMonth = days - (dayOfWeek(datetime(year, month, days)) - desiredWeekday + 7) % 7
    date(year, month, dayOfMonth)
  }

  def isAnyHoliday(date: LocalDate): Boolean = HolidayLookupFunctions.exists(holidayLookupFn => isHoliday(date, holidayLookupFn))

  /**
   * holidayFn is a function of an integer year that returns a LocalDate representing the date
   * that the holiday falls on in that year
   * Example: isHoliday(datetimeUtils(2012, 1, 16), martinLutherKingJrDay) => true
   */
  def isHoliday(date: LocalDate, holidayFn: (Int) => LocalDate): Boolean = date == holidayFn(date.getYear())

  val HolidayLookupFunctions = Vector[(Int) => LocalDate](
    newYears _,
    martinLutherKingJrDay _,
    presidentsDay _,
    goodFriday _,
    memorialDay _,
    independenceDay _,
    laborDay _,
    thanksgiving _,
    christmas _
  )

  def newYears(year: Int): LocalDate = date(year, 1, 1)

  // third monday in January in the given year
  def martinLutherKingJrDay(year: Int): LocalDate = nthWeekday(3, DateTimeConstants.MONDAY, 1, year)

  // third monday in February in the given year
  def presidentsDay(year: Int): LocalDate = nthWeekday(3, DateTimeConstants.MONDAY, 2, year)

  /**
   * This is a non-trivial calculation. See http://en.wikipedia.org/wiki/Computus
   *   "Computus (Latin for "computation") is the calculation of the date of Easter in the Christian calendar."
   *   Evidently the scientific study of computation (or Computer Science, as we like to call it) was born out
   *   of a need to calculate when easter was going to be.
   * See http://www.linuxtopia.org/online_books/programming_books/python_programming/python_ch38.html
   * There is also a clojure version (that doesn't work properly) at: http://www.bitshift.me/calculate-easter-in-clojure/
   * The following code was taken from: http://www.merlyn.demon.co.uk/estralgs.txt
   * function McClendon(YR) {
   *   var g, c, x, z, d, e, n
   *   g = YR % 19 + 1   // Golden
   *   c = ((YR/100)|0) + 1    // Century
   *   x = ((3*c/4)|0) - 12    // Solar
   *   z = (((8*c+5)/25)|0) - 5  // Lunar
   *   d = ((5*YR/4)|0) - x - 10 // Letter ?
   *   e = (11*g + 20 + z - x) % 30  // Epact
   *   if (e<0) e += 30    // Fix 9006 problem
   *   if ( ( (e==25) && (g>11) ) || (e==24) ) e++
   *   n = 44 - e
   *   if (n<21) n += 30   // PFM
   *   return n + 7 - ((d+n)%7)  // Following Sunday
   *   }
   */
  def easter(year: Int): LocalDate = {
    val g = year % 19 + 1
    val c = year / 100 + 1
    val x = (3 * c / 4) - 12
    val z = (8 * c + 5) / 25 - 5
    val d = 5 * year / 4 - x - 10
    val e = (11 * g + 20 + z - x) % 30
    val e1 = if (e < 0) {
      e + 30
    } else {
      e
    }
    val e2 = if ((e1 == 25 && g > 11) || e1 == 24) {
      e1 + 1
    } else {
      e1
    }
    val n = 44 - e2
    val n1 = if (n < 21) {
      n + 30
    } else {
      n
    }
    val n2 = (n1 + 7) - ((d + n1) % 7)
    val day = if (n2 > 31) n2 - 31 else n2
    val month = if (n2 > 31) 4 else 3
    date(year, month, day)
  }

  // the Friday before Easter Sunday
  def goodFriday(year: Int): LocalDate = easter(year).minus(Days.days(2))

  // last Monday in May
  def memorialDay(year: Int): LocalDate = lastWeekday(1, 5, year)

  // July 4th
  def independenceDay(year: Int): LocalDate = date(year, 7, 4)

  // first Monday in September
  def laborDay(year: Int): LocalDate = nthWeekday(1, DateTimeConstants.MONDAY, 9, year)

  // fourth Thursday in November
  def thanksgiving(year: Int): LocalDate = nthWeekday(4, DateTimeConstants.THURSDAY, 11, year)

  def christmas(year: Int): LocalDate = date(year, 12, 25)
}
