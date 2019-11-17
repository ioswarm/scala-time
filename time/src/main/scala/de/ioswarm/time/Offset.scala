package de.ioswarm.time

import java.time.{LocalDateTime, ZoneId, ZoneOffset}

object Offset {

  def apply(): Offset = Offset(0l)

  def apply(hours: Int, minutes: Int): Offset = Offset(
    (if (hours < 0 || minutes < 0) -1l else 1l) * (Math.abs(hours*HOUR_TO_MILLIS)+Math.abs(minutes*MINUTE_TO_MILLIS))
  )

  @throws[IllegalArgumentException]
  def apply(offset: String): Offset = offset match {
    case OFFSET_REGEX(_, z, _, hours, _, minutes) if z == "Z" => apply()
    case OFFSET_REGEX(_, z, _, hours, _, minutes) => apply(hours.toInt, minutes.toInt)
    case _ => throw new IllegalArgumentException(s"Could not parse '$offset'")
  }

  def apply(zoneOffset: ZoneOffset): Offset = apply(zoneOffset.getId)
  def apply(zoneId: ZoneId): Offset = apply(LocalDateTime.now().atZone(zoneId).getOffset)

}
case class Offset(millis: Long) {
  //require(millis >= -12*HOUR_TO_MILLIS && millis <= 12*HOUR_TO_MILLIS)

  lazy val asTuple: (Int, Int) = {
    val t = timeFromMillis(Math.abs(millis))
    (if (millis < 0) -1 else 1)*t._1 -> (if (millis < 0) -1 else 1)*t._2
  }

  def hours: Int = asTuple._1

  def minutes: Int = asTuple._2

  def toText: String = millis match {
    case 0l => "Z"
    case l: Long => s"${if (l < 0) "-" else "+"}%02d:%02d".format(Math.abs(hours),Math.abs(minutes))
  }

  override def toString: String = toText

  def +(h: Int): Offset = Offset(millis+h*HOUR_TO_MILLIS)
  def +(offset: Offset) = Offset(millis+offset.millis)
  def plus(h: Int): Offset = this.+(h)
  def plusMinutes(m: Int): Offset = Offset(millis+m*MINUTE_TO_MILLIS)
  def plusHours(h: Int): Offset = this.+(h)

  def -(h: Int): Offset = Offset(millis-h*HOUR_TO_MILLIS)
  def -(offset: Offset): Offset = Offset(millis-offset.millis)
  def minus(h: Int): Offset = this.-(h)
  def minusMinutes(m: Int): Offset = Offset(millis-m*MINUTE_TO_MILLIS)
  def minusHours(h: Int): Offset = this.-(h)

}