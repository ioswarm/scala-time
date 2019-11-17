package de.ioswarm.time

import scala.concurrent.duration.Duration

trait DateFacade[T <: DateFacade[T]] {

  def year: Int
  def month: Int
  def dayOfMonth: Int

  def +(d: Duration): T = this.+(d.toMillis)
  def +(millis: Long): T

  def -(d: Duration): T = this.-(d.toMillis)
  def -(millis: Long): T

  def plus(d: Duration): T = this.+(d)
  def plus(millis: Long): T = this.+(millis)

  def plusDays(days: Int): T = plus(days * DAY_TO_MILLIS)
  def plusMonths(months: Int): T = plus((0 until months).map(i => daysInMonth(year+(month-1 + i)/12, ((month-1 + i) % 12)+1)).sum*DAY_TO_MILLIS)
  def plusWeeks(weeks: Int): T = plus(weeks*7*DAY_TO_MILLIS)
  def plusYears(years: Int): T = plus((1 to years).map(i => daysInYear(i + year)).sum*DAY_TO_MILLIS)

  def minus(d: Duration): T = this.-(d)
  def minus(millis: Long): T = this.-(millis)

  def minusDays(days: Int): T = minus(days*DAY_TO_MILLIS)
  def minusMonths(months: Int): T = minus((0 until months).map(i => daysInMonth(year-(i+12-month)/12, (((i+12-month)/12)*12) + month-i)).sum*DAY_TO_MILLIS)
  def minusWeeks(weeks: Int): T = minus(weeks*7*DAY_TO_MILLIS)
  def minusYears(years: Int): T = minus((0 until years).map(i => daysInYear(year - i)).sum*DAY_TO_MILLIS)

  def dayOfYear: Int = dayOfYearNum(year, month, dayOfMonth)
  def dayOfWeek: Int = dayOfWeekNum(year, month, dayOfMonth)
  def calendarWeek: Int = calendarWeekNum(year, month, dayOfMonth)

}
object Date {

  def local(): Date = apply(Offsets.DEFAULT)

  def now(): Date = Date(System.currentTimeMillis(), Offsets.UTC)

  def apply(): Date = now()

  def apply(epoch: Long): Date = Date(epoch, Offsets.DEFAULT)

  def apply(offset: Offset): Date = Date(System.currentTimeMillis(), offset)

  def apply(year: Int, month: Int, dayOfMonth: Int): Date = Date(year, month, dayOfMonth, Offsets.DEFAULT)

  def apply(year: Int, month: Int, dayOfMonth: Int, offset: Offset): Date = {
    require(year >= 0 && year <= 9999)
    require(month >= 1 && month <= 12)
    require(dayOfMonth >= 1 && dayOfMonth <= monthDays(year)(month-1))

    Date(dayNumber(year, month, dayOfMonth) * DAY_TO_MILLIS, offset)
  }

  @throws[IllegalArgumentException]
  def apply(date: String): Date = date match {
    case DATE_REGEX(year, month, dayOfMonth) => apply(year.toInt, month.toInt, dayOfMonth.toInt)
    case _ => throw new IllegalArgumentException
  }

  def fromString(date: String): Date = apply(date)

}
case class Date(epoch: Long, offset: Offset) extends Temporal with DateFacade[Date] {

  lazy val asTuple: (Int, Int, Int) = dateFromMillis(time)

  override def year: Int = asTuple._1

  override def month: Int = asTuple._2

  override def dayOfMonth: Int = asTuple._3

  def toText: String = "%04d-%02d-%02d".format(year, month, dayOfMonth)

  override def toString: String = toText

  override def +(millis: Long): Date = Date(dayNumber(epoch+millis)*DAY_TO_MILLIS, offset)

  override def -(millis: Long): Date = if (millis < TIME_CONSTANT) this else Date(dayNumber(epoch-millis)*DAY_TO_MILLIS, offset)

  def and(time: Time): DateTime = DateTime(this, time, offset)
  def withTime(time: Time): DateTime = and(time)

  def toDateTime: DateTime = and(Time(offset))
}