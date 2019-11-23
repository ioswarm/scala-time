package de.ioswarm.time

object WeekDays {

  sealed abstract class Day(val dayNum: Int)

  case object SUNDAY extends Day(0)
  case object MONDAY extends Day(1)
  case object TUESDAY extends Day(2)
  case object WEDNESDAY extends Day(3)
  case object THURSDAY extends Day(4)
  case object FRIDAY extends Day(5)
  case object SATURDAY extends Day(6)

}
