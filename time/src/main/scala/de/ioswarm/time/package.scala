package de.ioswarm

import com.typesafe.config.ConfigFactory

package object time {

  private[time] final val TIME_MATCH = "([2][0-3]|[0-1][0-9]):([0-5][0-9])(:([0-5][0-9])(\\.([0-9]{1,3}))?)?"
  private[time] final val TIME_REGEX = TIME_MATCH.r
  private[time] final val DATE_MATCH = "([0-9]{4})\\-([1][0-2]|[0][1-9])\\-([3][0-1]|[1-2][0-9]|[0][1-9])"
  private[time] final val DATE_REGEX = DATE_MATCH.r
  private[time] final val OFFSET_MATCH = "((Z)|(([+,-]([1][0-4]|0[0-9])):([0-5][0-9])))"
  private[time] final val OFFSET_REGEX = OFFSET_MATCH.r
  private[time] final val OFFSET_DATETIME_MATCH = DATE_MATCH+"T"+TIME_MATCH+OFFSET_MATCH
  private[time] final val OFFSET_DATETIME_REGEX = OFFSET_DATETIME_MATCH.r
  private[time] final val DATETIME_MATCH = DATE_MATCH+"T"+TIME_MATCH+"Z"
  private[time] final val DATETIME_REGEX = DATETIME_MATCH.r

  private[time] final val TIME_CONSTANT = 86399999l

  final val SECOND_TO_MILLIS: Long = 1000l
  final val MINUTE_TO_MILLIS: Long = 60l * SECOND_TO_MILLIS
  final val HOUR_TO_MILLIS: Long = 60l * MINUTE_TO_MILLIS
  final val DAY_TO_MILLIS: Long = 24l * HOUR_TO_MILLIS

  def dayNumber(millis: Long): Int = (millis /  DAY_TO_MILLIS).toInt

  def dayNumber(year: Int, month: Int, dayOfMonth: Int): Int = {
    val y = if (month <= 2) year-1 else year
    val era = (if (y >= 0) y else y-399) / 400
    val yoe = y - era * 400
    val doy = (153*(month + (if (month > 2) -3 else 9)) + 2)/5 + dayOfMonth-1
    val doe = yoe * 365 + yoe/4 - yoe/100 + doy
    era * 146097 + doe - 719468
  }

  def timeNumber(millis: Long): Int = (millis - millis/DAY_TO_MILLIS*DAY_TO_MILLIS).toInt
  def timeNumber(hour: Int, minute: Int, second: Int, millis: Int): Int = (hour*HOUR_TO_MILLIS + minute*MINUTE_TO_MILLIS + second*SECOND_TO_MILLIS + millis).toInt

  def dateFromDayNumber(dayNumber: Int): (Int, Int, Int) = {
    val z = dayNumber+719468
    val era = (if (z >= 0) z else z - 146096) / 146097
    val doe = z - era * 146097
    val yoe = (doe - doe/1460 + doe/36524 - doe/146096) / 365
    val y = yoe + era * 400
    val doy = doe - (365*yoe + yoe/4 - yoe/100)
    val mp = (5*doy + 2)/153
    val d = doy - (153*mp+2)/5 + 1
    val m = mp + (if (mp < 10) 3 else -9)
    (y + (if (m <= 2) 1 else 0), m, d)
  }

  def timeFromTimeNumber(timeNumber: Int): (Int, Int, Int, Int) = {
    val epoch = timeNumber.toLong
    val hour: Int = ((epoch - epoch/DAY_TO_MILLIS*DAY_TO_MILLIS) / HOUR_TO_MILLIS).toInt
    val minute: Int = ((epoch - epoch/DAY_TO_MILLIS*DAY_TO_MILLIS - (hour * HOUR_TO_MILLIS)) / MINUTE_TO_MILLIS).toInt
    val second: Int = ((epoch - epoch/DAY_TO_MILLIS*DAY_TO_MILLIS - (hour * HOUR_TO_MILLIS) - (minute * MINUTE_TO_MILLIS)) / SECOND_TO_MILLIS).toInt
    val millis: Int = (epoch - epoch/DAY_TO_MILLIS*DAY_TO_MILLIS - (hour * HOUR_TO_MILLIS) - (minute * MINUTE_TO_MILLIS) - (second * SECOND_TO_MILLIS)).toInt
    (hour, minute, second, millis)
  }

  def dateFromMillis(millis: Long): (Int, Int, Int) = dateFromDayNumber(dayNumber(millis))

  def timeFromMillis(millis: Long): (Int, Int, Int, Int) = timeFromTimeNumber(timeNumber(millis))

  def leapModifier(year: Int): Int = if ((year % 4 == 0) && ((year < 1582) || (!(year % 100 == 0)) || (year % 400 == 0))) 1 else 0
  def isLeapYear(year: Int): Boolean = leapModifier(year) == 1

  def monthDays(year: Int): Array[Int] = Array(31,28+leapModifier(year),31,30,31,30,31,31,30,31,30,31)

  def daysInYear(year: Int): Int = 365+leapModifier(year)//monthDays(year).sum

  def daysInMonth(year: Int, month: Int): Int = {
    require(month > 0 && month <= 12, "Month must between 1 and 12.")
    monthDays(year)(month-1)
  }

  def dayOfYearNum(year: Int, month: Int, dayOfMonth: Int): Int = {
    val mds = monthDays(year)
    require(month > 0 && month <= 12, "Month must between 1 and 12.")
    require(dayOfMonth<=mds(month-1))
    monthDays(year).take(month-1).sum+dayOfMonth
  }

  def doomDay(year: Int): Int = {
    val cdd = Array(5,4,2,0)
    val cc: Int = year/100
    val yy = year%100
    val ccDoomDay = cdd(cc%4)
    val ret = if (yy == 0) ccDoomDay else if (yy%12 == 0) (ccDoomDay+yy/12-1)%7 else (ccDoomDay+yy/12+yy%12+((yy-1)%12)/4)%7
    if (cc%4 == 0 && yy != 0) (ret+1)%7 else ret
  }

  def dayOfWeekNum(year: Int, month: Int, dayOfMonth: Int): Int = (doomDay(year)+dayOfYearNum(year, month, dayOfMonth)%7)%7

  def calendarWeekNum(year: Int, month: Int, dayOfMonth: Int): Int = {
    val days = dayOfYearNum(year, month, dayOfMonth) + (if (doomDay(year) > 3) -1*(7-doomDay(year)) else doomDay(year))
    val week = if (days < 0) {
      if (doomDay(year) == 4 || doomDay(year-1) == 3) 53 else 52
    } else days/7+1
    if (days > 360 && week > 52) {
      if (doomDay(year) == 3) 53
      else if (doomDay(year+1) == 4) 53
      else 1
    } else week
  }

  private def easterFunc(year: Int): Int = {
    val M: Int = 15 + (if (year > 1582) (3*year/100+3)/4 - (8*year/100+13)/25 else 0)
    val S: Int = if (year > 1582) 2-(3*year/100+3)/4 else 0
    val A = year%19
    val D = (19*A+M)%30
    val R: Int = (D+A/11)/29
    val OG = 21+D-R
    OG + (7 - (OG-(7 - (year+year/4+S)%7))%7)
  }

  private lazy val config = ConfigFactory.load().getConfig("ioswarm.time")
  lazy val timeFormat: String = config.getString("timeFormat")
  lazy val dateFormat: String = config.getString("dateFormat")
  lazy val dateTimeFormat: String = config.getString("dateTimeFormat")


  def easterDate(year: Int): Date = Date(year,3,1) plusDays (easterFunc(year)-1)
  def firstOfYear(year: Int): Date = Date(year, 1, 1)
  def lastOfYear(year: Int): Date = Date(year, 12, 1)
  def firstOfMonth(year: Int, month: Int): Date = Date(year,month,1)
  def lastOfMonth(year: Int, month: Int): Date = Date(year,month,daysInMonth(year,month))
  def firstOfWeek(year: Int, week: Int): Date = ???
  def lastOfWeek(year: Int, week: Int): Date = ???


  // implicit conversions
  implicit def _dateToString(d: Date): String = Formatter.format(dateFormat, d)
  implicit def _dateTimeToString(dt: DateTime): String = Formatter.format(dateTimeFormat, dt)
  implicit def _timeToString(t: Time): String = Formatter.format(timeFormat, t)

  implicit def _stringToDate(s: String): Date = Formatter.parseDate(dateTimeFormat, s)
  implicit def _stringToTime(s: String): Time = Formatter.parseTime(timeFormat, s)
  implicit def _stringToDateTime(s: String): DateTime = Formatter.parse(dateTimeFormat, s)

  import java.util.{Date => UDate}
  import java.sql.{Date => SDate, Time => STime, Timestamp => STimestamp}

  implicit def _dateToUtilDate(d: Date): UDate = new UDate(d.epoch)
  implicit def _dateTimeToUtilDate(dt: DateTime) = new UDate(dt.epoch)

  implicit def _dateToSQLDate(d: Date): SDate = new SDate(d.epoch)
  implicit def _dateTimeToSQLDate(dt: DateTime): SDate = new SDate(dt.epoch)

  implicit def _dateToSQLTimestamp(d: Date): STimestamp = new STimestamp(d.epoch)
  implicit def _dateTimeToSQLTimestamp(dt: DateTime): STimestamp = new STimestamp((dt.epoch))

  implicit def _timeToSQLTime(t: Time): STime = new STime(t.epoch)


  implicit def _utilDateToDate(ud: UDate): Date = Date(ud.getTime)
  implicit def _utilDateToDateTime(ud: UDate): DateTime = DateTime(ud.getTime)

  implicit def _sqlDateToDate(sd: SDate): Date = Date(sd.getTime)
  implicit def _sqlDateToDateTime(sd: SDate): DateTime = DateTime(sd.getTime)

  implicit def _sqlTimestampToDate(st: STimestamp): Date = Date(st.getTime)
  implicit def _sqlTimestampToDateTime(st: STimestamp): DateTime = DateTime(st.getTime)

  implicit def _sqlTimeToTime(st: STime): Time = Time(st.getTime)

}

