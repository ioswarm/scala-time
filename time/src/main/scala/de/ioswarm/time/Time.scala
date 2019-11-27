package de.ioswarm.time

import com.google.protobuf.wrappers.Int64Value
import scalapb.TypeMapper

import concurrent.duration.Duration

trait TimeFacade[T <: TimeFacade[T]] {

  def hour: Int
  def minute: Int
  def second: Int
  def millis: Int

  def +(d: Duration): T = this.+(d.toMillis)
  def +(millis: Long): T

  def -(d: Duration): T = this.-(d.toMillis)
  def -(millis: Long): T

  def plus(d: Duration): T = this.+(d)
  def plus(millis: Long): T = this.+(millis)

  def plusHours(hour: Int): T = this.+(hour*HOUR_TO_MILLIS)
  def plusMinutes(minutes: Int): T = this.+(minutes*MINUTE_TO_MILLIS)
  def plusSeconds(seconds: Int): T = this.+(seconds*SECOND_TO_MILLIS)

  def minusHours(hour: Int): T = this.-(hour*HOUR_TO_MILLIS)
  def minusMinutes(minutes: Int): T = this.-(minutes*MINUTE_TO_MILLIS)
  def minusSeconds(seconds: Int): T = this.-(seconds*SECOND_TO_MILLIS)

  def withHour(h: Int): T
  def withMinute(m: Int): T
  def withSecond(s: Int): T
  def withMillis(S: Int): T

  def withOffset(o: Offset): T

  def map[B](f: T => B): B
}

object Time {

  def utc: Time = apply(Offsets.UTC)

  def now: Time = Time(timeNumber(System.currentTimeMillis()).toLong, Offsets.LOCAL)

  def apply(): Time = now

  def apply(epoch: Long): Time = Time(epoch, Offsets.LOCAL)

  def apply(offset: Offset): Time = Time(timeNumber(System.currentTimeMillis()).toLong, offset)

  def apply(hour: Int, minute: Int, second: Int, millis: Int): Time = Time(hour, minute, second, millis, Offsets.LOCAL)

  def apply(hour: Int, minute: Int, second: Int, millis: Int, offset: Offset): Time = {
    require(hour >= 0 && hour <= 23)
    require(minute >= 0 && minute <= 59)
    require(second >= 0 && second <= 59)
    require(millis >= 0 && millis <= 999)

    Time(timeNumber(hour, minute, second, millis).toLong, offset)
  }

  def apply(hour: Int, minute: Int, second: Int): Time = apply(hour, minute, second, 0)
  def apply(hour: Int, minute: Int): Time = apply(hour, minute, 0, 0)

  def apply(time: String, format: String): Time = Formatter.parseTime(format, time)
  def apply(time: String): Time = apply(time, timeFormat)

  implicit val _timeTypeMapper: TypeMapper[Long, Time] = TypeMapper((l: Long) => Time(l, Offsets.LOCAL))(_.epoch)

  val _timeInt64WrapperTypeMapper: TypeMapper[Int64Value, Time] = TypeMapper((w:Int64Value) => Time(w.value, Offsets.LOCAL))(t => Int64Value(t.epoch))

}
case class Time(epoch: Long, offset: Offset) extends Temporal with TimeFacade[Time] {

  lazy val asTuple: (Int, Int, Int, Int) = timeFromMillis(time)

  override def hour: Int = asTuple._1

  override def minute: Int = asTuple._2

  override def second: Int = asTuple._3

  override def millis: Int = asTuple._4

  override def toString: String = this

  def +(millis: Long): Time = {
    require(epoch+millis <= TIME_CONSTANT, "Time could not greater than 23:59:59.999")
    Time(epoch+millis, offset)
  }

  def -(millis: Long): Time = {
    require(epoch-millis > 0l, "Time could not less than 00:00:00.000")
    Time(epoch-millis, offset)
  }

  def and(date: Date): DateTime = DateTime(date, this, offset)
  def withDate(date: Date): DateTime = and(date)

  def toDateTime: DateTime = and(Date(offset))

  override def withHour(h: Int): Time = Time(timeNumber(h, minute, second, millis).toLong, offset)

  override def withMinute(m: Int): Time = Time(timeNumber(hour, minute, second, millis).toLong, offset)

  override def withSecond(s: Int): Time = Time(timeNumber(hour, minute, second, millis).toLong, offset)

  override def withMillis(S: Int): Time = Time(timeNumber(hour, minute, second, millis).toLong, offset)

  override def withOffset(o: Offset): Time = Time(epoch, o)

  override def map[B](f: Time => B): B = f(this)

}