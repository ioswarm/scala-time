package de.ioswarm.time

object Calendar {

  sealed trait Crawler[T] {

    def +(n: Int): T = next(n)
    def ++(): T = next(1)
    def next(n: Int): T

    def -(n: Int): T = prev(n)
    def --(): T = prev(1)
    def prev(n: Int): T
  }

  final case class Year(year: Int) extends Crawler[Year] {
    override def next(n: Int = 1): Year = Year(year+n)
    override def prev(n: Int = 1): Year = Year(year-n)
  }

  object Month {
    def apply[T <: DateFacade[T]](d: DateFacade[T]): Month = Month(d.year, d.month)
  }
  final case class Month(year: Int, month: Int) extends Crawler[Month] {
    require(month >= 1 && month <= 12, "Month must between 1 and 12")

    override def next(n: Int = 1): Month = Month(firstDay plusMonths n)

    override def prev(n: Int = 1): Month = Month(firstDay minusMonths n)

    def firstDay: Date = Date(year, month, 1)

    def lastDay: Date = Date(year, month, daysInMonth(year, month))

    override def toString: String = f"$year-$month%02d"
  }


  object Week {
    def apply(d: Date): Week = Week(d.year, d.calendarWeek)
  }
  final case class Week(year: Int, week: Int) extends Crawler[Week] {

    override def next(n: Int = 1): Week = Week(firstDay plusDays n*7)

    override def prev(n: Int = 1): Week = Week(firstDay minusDays n*7)

    def firstDay: Date = {
      val dd = Date(year, 1, 1)
      dd.map{d => d.calendarWeek match {
        case 1 => dd plusDays (week-1)*7
        case _ => dd plusDays week*7
      }}.map{d => d minusDays d.dayOfWeek-1}
    }

    def lastDay: Date = firstDay plusDays 6
  }

  object Day {
    def apply(d: Date): Day = Day(d.year, d.month, d.dayOfMonth)
  }
  final case class Day(year: Int, month: Int, dayOfMonth: Int) extends Crawler[Day] {
    require(month >= 1 && month <= 12, "Month must between 1 and 12")
    private val lastDay = monthDays(year)(month-1)
    require(dayOfMonth >= 1 && dayOfMonth <= lastDay, s"Day of month must between 1 and $lastDay")

    override def next(n: Int = 1): Day = Day(date plusDays n)

    override def prev(n: Int = 1): Day = Day(date minusDays n)

    def date: Date = Date(year, month, dayOfMonth)
  }

  object Hour {
    def apply(dt: DateTime): Hour = Hour(dt.year, dt.month, dt.dayOfMonth, dt.hour)
  }
  final case class Hour(year: Int, month: Int, dayOfMonth: Int, hour: Int) extends Crawler[Hour] {
    require(month >= 1 && month <= 12, "Month must between 1 and 12")
    private val lastDay = monthDays(year)(month-1)
    require(dayOfMonth >= 1 && dayOfMonth <= lastDay, s"Day of month must between 1 and $lastDay")
    require(hour >= 0 && hour <= 23, "Hour must between 0 and 23")

    override def next(n: Int = 1): Hour = Hour(dateTime plusHours n)

    override def prev(n: Int = 1): Hour = Hour(dateTime minusHours n)

    def dateTime: DateTime = DateTime(year, month, dayOfMonth, hour, 0, 0, 0)

    def date: Date = dateTime.toDate
    def time: Time = dateTime.toTime
  }

  object Minute{
    def apply(dt: DateTime): Minute = Minute(dt.year, dt.month, dt.dayOfMonth, dt.hour, dt.minute)
  }
  final case class Minute(year: Int, month: Int, dayOfMonth: Int, hour: Int, minute: Int) extends Crawler[Minute] {
    require(month >= 1 && month <= 12, "Month must between 1 and 12")
    private val lastDay = monthDays(year)(month-1)
    require(dayOfMonth >= 1 && dayOfMonth <= lastDay, s"Day of month must between 1 and $lastDay")
    require(hour >= 0 && hour <= 23, "Hour must between 0 and 23")
    require(minute >= 0 && minute <= 59, "Minute must between 0 and 59")

    override def next(n: Int = 1): Minute = Minute(dateTime plusMinutes n)

    override def prev(n: Int = 1): Minute = Minute(dateTime minusMinutes n)

    def dateTime: DateTime = DateTime(year, month, dayOfMonth, hour, minute, 0, 0)

    def date: Date = dateTime.toDate
    def time: Time = dateTime.toTime
  }

}
