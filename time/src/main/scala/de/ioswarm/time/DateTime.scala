package de.ioswarm.time

import com.google.protobuf.wrappers.Int64Value
import scalapb.TypeMapper

import scala.concurrent.duration.Duration

trait DateTimeFacade[T <: DateTimeFacade[T]] extends DateFacade[T] with TimeFacade[T] {

  override def +(d: Duration): T = this.+(d.toMillis)

  override def -(d: Duration): T = this.-(d.toMillis)

  override def plus(d: Duration): T = this.+(d)
  override def plus(millis: Long): T = this.+(millis)

  override def minus(d: Duration): T = this.-(d)
  override def minus(millis: Long): T = this.-(millis)

}
object DateTime {

  def utc: DateTime = DateTime(System.currentTimeMillis(), Offsets.UTC)

  def now: DateTime = DateTime(System.currentTimeMillis(), Offsets.LOCAL)

  def apply(): DateTime = now

  def apply(epoch: Long): DateTime = DateTime(epoch, Offsets.LOCAL)

  def apply(offset: Offset): DateTime = DateTime(System.currentTimeMillis(), offset)

  def apply(year: Int, month: Int, dayOfMonth: Int, hour: Int, minute: Int, second: Int, millis: Int, offset: Offset): DateTime = {
    require(year >= 0 && year <= 9999)
    require(month >= 1 && month <= 12)
    require(dayOfMonth >= 1 && dayOfMonth <= monthDays(year)(month-1))
    require(hour >= 0 && hour <= 23)
    require(minute >= 0 && minute <= 59)
    require(second >= 0 && second <= 59)
    require(millis >= 0 && millis <= 999)

    DateTime(dayNumber(year, month, dayOfMonth)*DAY_TO_MILLIS+timeNumber(hour, minute, second, millis), offset)
  }

  def apply(year: Int, month: Int, dayOfMonth: Int, hour: Int, minute: Int, second: Int, millis: Int): DateTime = DateTime(
    year
    , month
    , dayOfMonth
    , hour
    , minute
    , second
    , millis
    , Offsets.DEFAULT
  )

  def apply(year: Int, month: Int, dayOfMonth: Int, hour: Int, minute: Int, second: Int, offset: Offset): DateTime = apply(year, month, dayOfMonth, hour, minute, second, 0, offset)

  def apply(year: Int, month: Int, dayOfMonth: Int, hour: Int, minute: Int, second: Int): DateTime = apply(year, month, dayOfMonth, hour, minute, second, 0)

  def apply(year: Int, month: Int, dayOfMonth: Int, hour: Int, minute: Int, offset: Offset): DateTime = apply(year, month, dayOfMonth, hour, minute, 0, 0, offset)

  def apply(year: Int, month: Int, dayOfMonth: Int, hour: Int, minute: Int): DateTime = apply(year, month, dayOfMonth, hour, minute, 0, 0)

  def apply(year: Int, month: Int, dayOfMonth: Int, hour: Int, offset: Offset): DateTime = apply(year, month, dayOfMonth, hour, 0, 0, 0, offset)

  def apply(year: Int, month: Int, dayOfMonth: Int, hour: Int): DateTime = apply(year, month, dayOfMonth, hour, 0, 0, 0)

  def apply(year: Int, month: Int, dayOfMonth: Int, offset: Offset): DateTime = apply(year, month, dayOfMonth, 0, 0, 0, 0, offset)

  def apply(year: Int, month: Int, dayOfMonth: Int): DateTime = apply(year, month, dayOfMonth, 0, 0, 0, 0)

  def apply(dt: String, format: String): DateTime = Formatter.parse(format, dt)
  def apply(dt: String): DateTime = apply(dt, dateTimeFormat)


  def apply(dt: (Date, Time), offset: Offset): DateTime = apply(dt._1, dt._2, offset)
  def apply(dt: (Date, Time)): DateTime = apply(dt._1, dt._2)
  def apply(date: Date, time: Time, offset: Offset): DateTime = apply(date.epoch+time.epoch, offset)
  def apply(date: Date, time: Time): DateTime = apply(date.epoch+time.epoch, date.offset)
  def apply(date: Date, offset: Offset): DateTime = apply(date.epoch, offset)
  def apply(date: Date): DateTime = apply(date.epoch, date.offset)
  def apply(time: Time, offset: Offset): DateTime = apply(time.epoch, offset)
  def apply(time: Time): DateTime = apply(time.epoch, time.offset)

  implicit val _dateTimeTypeMapper: TypeMapper[Long, DateTime] = TypeMapper((l: Long) => DateTime(l, Offsets.LOCAL))(_.epoch)

  implicit val _dateTimeInt64WrapperTypeMapper: TypeMapper[Int64Value, DateTime] = TypeMapper((w:Int64Value) => DateTime(w.value, Offsets.LOCAL))(d => Int64Value(d.epoch))
}
case class DateTime(epoch: Long, offset: Offset) extends Temporal with DateTimeFacade[DateTime] {

  lazy val asTimeTuple: (Int, Int, Int, Int) = timeFromMillis(time)
  lazy val asDateTuple: (Int, Int, Int) = dateFromMillis(time)

  override def year: Int = asDateTuple._1

  override def month: Int = asDateTuple._2

  override def dayOfMonth: Int = asDateTuple._3

  override def hour: Int = asTimeTuple._1

  override def minute: Int = asTimeTuple._2

  override def second: Int = asTimeTuple._3

  override def millis: Int = asTimeTuple._4

  override def toString: String = this

  override def +(millis: Long): DateTime = DateTime(epoch+millis, offset)
  override def -(millis: Long): DateTime = DateTime(epoch-millis, offset)

  def toDate: Date = Date(year, month, dayOfMonth, offset)
  def toTime: Time = Time(hour, minute, second, millis, offset)

  override def withYear(y: Int): DateTime = DateTime(dayNumber(y, month, dayOfMonth)*DAY_TO_MILLIS+timeNumber(hour, minute, second, millis)-offset.millis, offset)

  override def withMonth(m: Int): DateTime = DateTime(dayNumber(year, m, dayOfMonth)*DAY_TO_MILLIS+timeNumber(hour, minute, second, millis)-offset.millis, offset)

  override def withDayOfMonth(d: Int): DateTime = DateTime(dayNumber(year, month, d)*DAY_TO_MILLIS+timeNumber(hour, minute, second, millis)-offset.millis, offset)

  override def withHour(h: Int): DateTime = DateTime(dayNumber(year, month, dayOfMonth)*DAY_TO_MILLIS+timeNumber(h, minute, second, millis)-offset.millis, offset)

  override def withMinute(m: Int): DateTime = DateTime(dayNumber(year, month, dayOfMonth)*DAY_TO_MILLIS+timeNumber(hour, m, second, millis)-offset.millis, offset)

  override def withSecond(s: Int): DateTime = DateTime(dayNumber(year, month, dayOfMonth)*DAY_TO_MILLIS+timeNumber(hour, minute, s, millis)-offset.millis, offset)

  override def withMillis(S: Int): DateTime = DateTime(dayNumber(year, month, dayOfMonth)*DAY_TO_MILLIS+timeNumber(hour, minute, second, S)-offset.millis, offset)

  override def withOffset(o: Offset): DateTime = DateTime(epoch, o)

  override def map[B](f: DateTime => B): B = f(this)

}