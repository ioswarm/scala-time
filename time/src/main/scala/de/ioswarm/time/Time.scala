package de.ioswarm.time

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

}

object Time {

  def local(): Time = apply(Offsets.DEFAULT)

  def now(): Time = Time(timeNumber(System.currentTimeMillis()).toLong, Offsets.UTC)

  def apply(): Time = now()

  def apply(epoch: Long): Time = Time(epoch, Offsets.DEFAULT)

  def apply(offset: Offset): Time = Time(timeNumber(System.currentTimeMillis()).toLong, offset)

  def apply(hour: Int, minute: Int, second: Int, millis: Int): Time = Time(hour, minute, second, millis, Offsets.DEFAULT)

  def apply(hour: Int, minute: Int, second: Int, millis: Int, offset: Offset): Time = {
    require(hour >= 0 && hour <= 23)
    require(minute >= 0 && minute <= 59)
    require(second >= 0 && second <= 59)
    require(millis >= 0 && millis <= 999)

    Time(timeNumber(hour, minute, second, millis).toLong, offset)
  }

  def apply(hour: Int, minute: Int, second: Int): Time = apply(hour, minute, second, 0)
  def apply(hour: Int, minute: Int): Time = apply(hour, minute, 0, 0)

  @throws[IllegalArgumentException]
  def apply(time: String): Time = time match {
    case TIME_REGEX(hour, minute, _, second, _, millis) if millis == null && second == null => apply(hour.toInt, minute.toInt)
    case TIME_REGEX(hour, minute, _, second, _, millis) if millis == null => apply(hour.toInt, minute.toInt, second.toInt)
    case TIME_REGEX(hour, minute, _, second, _, millis) => apply(hour.toInt, minute.toInt, second.toInt, millis.toInt)
    case _ => throw new IllegalArgumentException
  }

  def fromString(time: String): Time = apply(time)

}
case class Time(epoch: Long, offset: Offset) extends Temporal with TimeFacade[Time] {

  lazy val asTuple: (Int, Int, Int, Int) = timeFromMillis(time)

  override def hour: Int = asTuple._1

  override def minute: Int = asTuple._2

  override def second: Int = asTuple._3

  override def millis: Int = asTuple._4

  def toShortText: String = "%02d:%02d".format(hour, minute)
  def toMediumText: String = "%02d:%02d:%02d".format(hour, minute, second)
  def toLongText: String = "%02d:%02d:%02d.%03d".format(hour, minute, second, millis)

  override def toString: String = toLongText

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

}