package de.ioswarm.time

import com.google.protobuf.wrappers.Int64Value
import scalapb.TypeMapper

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
  def weekDay: WeekDays.Day = dayOfWeek match {
    case 1 => WeekDays.MONDAY
    case 2 => WeekDays.TUESDAY
    case 3 => WeekDays.WEDNESDAY
    case 4 => WeekDays.THURSDAY
    case 5 => WeekDays.FRIDAY
    case 6 => WeekDays.SATURDAY
    case _ => WeekDays.SUNDAY
  }
  def calendarWeek: Int = calendarWeekNum(year, month, dayOfMonth)

  def withYear(y: Int): T
  def withMonth(m: Int): T
  def withDayOfMonth(d: Int): T

  def withOffset(o: Offset): T

  def map[B](f: T => B): B

}
object Date {

  def utc: Date = apply(Offsets.UTC)

  def now: Date = Date(System.currentTimeMillis(), Offsets.LOCAL)

  def apply(): Date = now

  def apply(epoch: Long): Date = Date(epoch, Offsets.LOCAL)

  def apply(offset: Offset): Date = Date(System.currentTimeMillis(), offset)

  def apply(year: Int, dayOfYear: Int, offset: Offset): Date = {
    val dd = Date(year, 1, 1, offset)
    dd plusDays dayOfYear - 1
  }

  def apply(year: Int, dayOfYear: Int): Date = apply(year, dayOfYear, Offsets.LOCAL)

  def apply(year: Int, month: Int, dayOfMonth: Int): Date = Date(year, month, dayOfMonth, Offsets.LOCAL)

  def apply(year: Int, month: Int, dayOfMonth: Int, offset: Offset): Date = {
    require(year >= 0 && year <= 9999)
    require(month >= 1 && month <= 12)
    require(dayOfMonth >= 1 && dayOfMonth <= monthDays(year)(month-1))

    Date(dayNumber(year, month, dayOfMonth) * DAY_TO_MILLIS, offset)
  }

  def apply(date: String, format: String): Date = Formatter.parseDate(format, date)
  def apply(date: String): Date = apply(date, dateFormat)

  implicit val _dateLongTypeMapper: TypeMapper[Long, Date] = TypeMapper((l:Long) => Date(l, Offsets.LOCAL))(_.epoch)

  implicit val _dateInt64WrapperTypeMapper: TypeMapper[Int64Value, Date] = TypeMapper((w:Int64Value) => Date(w.value, Offsets.LOCAL))(d => Int64Value(d.epoch))
}
case class Date(epoch: Long, offset: Offset) extends Temporal with DateFacade[Date] with Ordered[Date] {

  lazy val asTuple: (Int, Int, Int) = dateFromMillis(time)

  override def year: Int = asTuple._1

  override def month: Int = asTuple._2

  override def dayOfMonth: Int = asTuple._3

  override def toString: String = this

  override def +(millis: Long): Date = Date(dayNumber(epoch+millis)*DAY_TO_MILLIS, offset)

  override def -(millis: Long): Date = if (millis < TIME_CONSTANT) this else Date(dayNumber(epoch-millis)*DAY_TO_MILLIS, offset)

  def and(time: Time): DateTime = DateTime(this, time, offset)
  def withTime(time: Time): DateTime = and(time)

  def toDateTime: DateTime = and(Time(offset))

  override def withYear(y: Int): Date = Date(dayNumber(y, month, dayOfMonth)*DAY_TO_MILLIS, offset)

  override def withMonth(m: Int): Date = Date(dayNumber(year, m, dayOfMonth)*DAY_TO_MILLIS, offset)

  override def withDayOfMonth(d: Int): Date = Date(dayNumber(year, month, d)*DAY_TO_MILLIS, offset)

  override def withOffset(o: Offset): Date = Date(epoch, o)

  override def map[B](f: Date => B): B = f(this)

  override def compare(that: Date): Int = (epoch - that.epoch).toInt

}