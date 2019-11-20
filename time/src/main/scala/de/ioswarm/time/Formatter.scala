package de.ioswarm.time

object Formatter {

  def format(format: String, temporal: Temporal): String = {
    def extractLastN(value: Int, length: Int): Int = {
      if (value.toDouble < math.pow(10.0, length)) value else (value % math.pow(10.0, length)).toInt
    }

    val epoch = "(h{1,2})".r.findFirstMatchIn(format) match {
      case Some(_) => temporal.time
      case None => temporal.epoch + Offsets.DEFAULT.millis
    }
    val dateT = dateFromMillis(epoch)
    val timeT = timeFromMillis(epoch)

    ("(y{1,4})".r.findAllMatchIn(format).map(_.group(0)).toList.sortWith((s1, s2) => s1.length > s2.length).distinct.map{
      case pat @ "y" => pat -> f"${dateT._1}%d"
      case pat @ "yy" => pat -> f"${extractLastN(dateT._1, 2)}%02d"
      case pat @ _ => pat -> f"${extractLastN(dateT._1, 4)}%04d"
    } ++
      "(M{1,2})".r.findAllMatchIn(format).map(_.group(0)).toList.sortWith((s1, s2) => s1.length > s2.length).distinct.map{
        case pat @ "MM" => pat -> f"${extractLastN(dateT._2, 2)}%02d"
        case pat @ _ => pat -> f"${dateT._2}%d"
      } ++
      "(d{1,2})".r.findAllMatchIn(format).map(_.group(0)).toList.sortWith((s1, s2) => s1.length > s2.length).distinct.map{
        case pat @ "dd" => pat -> f"${extractLastN(dateT._3, 2)}%02d"
        case pat @ _ => pat -> f"${dateT._3}%d"
      } ++
      "(H{1,2}|h{1,2})".r.findAllMatchIn(format).map(_.group(0)).toList.sortWith((s1, s2) => s1.length > s2.length).distinct.map{
        case pat @ "HH" => pat -> f"${extractLastN(timeT._1, 2)}%02d"
        case pat @ "hh" => pat -> f"${extractLastN(timeT._1, 2)}%02d"
        case pat @ _ => pat -> f"${timeT._1}%d"
      } ++
      "(m{1,2})".r.findAllMatchIn(format).map(_.group(0)).toList.sortWith((s1, s2) => s1.length > s2.length).distinct.map{
        case pat @ "mm" => pat -> f"${extractLastN(timeT._2, 2)}%02d"
        case pat @ _ => pat -> f"${timeT._2}%d"
      } ++
      "(s{1,2})".r.findAllMatchIn(format).map(_.group(0)).toList.sortWith((s1, s2) => s1.length > s2.length).distinct.map{
        case pat @ "ss" => pat -> f"${extractLastN(timeT._3, 2)}%02d"
        case pat @ _ => pat -> f"${timeT._3}%d"
      } ++
      "(S{1,2})".r.findAllMatchIn(format).map(_.group(0)).toList.sortWith((s1, s2) => s1.length > s2.length).distinct.map{
        case pat @ "SSS" => pat -> f"${extractLastN(timeT._4, 3)}%03d"
        case pat @ "SS" => pat -> f"${extractLastN(timeT._4, 2)}%02d"
        case pat @ _ => pat -> f"${timeT._4}%d"
      } ++
      List("(Z)".r.findFirstMatchIn(format).map(_.group(0) -> temporal.offset.toString)).flatten)
      .foldLeft(format){(format, t) => format.replaceAll(t._1, t._2)}
      .replace("Z", temporal.offset.toText)
  }

  def parse(format: String, value: String): DateTime = Some(List(
    ("yyyy", "(\\[0-9]{4})", "<10A>")
    , ("yy", "(\\[0-9]{2})", "<10B>")
    , ("y", "(\\[0-9]+)", "<10C>")
    , ("MM", "(\\[0-9]{2})", "<10D>")
    , ("M", "(\\[0-9]+)", "<10E>")
    , ("dd", "(\\[0-9]{2})", "<10F>")
    , ("d", "(\\[0-9]+)", "<111>")
    , ("HH", "(\\[0-9]{2})", "<112>")
    , ("H", "(\\[0-9]+)", "<113>")
    , ("mm", "(\\[0-9]{2})", "<114>")
    , ("m", "(\\[0-9]+)", "<115>")
    , ("ss", "(\\[0-9]{2})", "<116>")
    , ("s", "(\\[0-9]+)", "<117>")
    , ("SSS", "(\\[0-9]{3})", "<118>")
    , ("SS", "(\\[0-9]{2})", "<119>")
    , ("S", "(\\[0-9]+)", "<11A>")
    , ("Z", "((?:Z)|(?:(?:[-+])(?:[0-9]{2}):(?:[0-9]{2})))", "<11B>")
  ).foldLeft(format, format){(tf, tm) => tf._1.replaceAll(tm._1, tm._2) -> tf._2.replaceAll(tm._1, tm._3)}).flatMap{ vt =>
    vt._1.r.findFirstMatchIn(value).map(m => (0 to m.groupCount).map(m.group).drop(1)).map{vals =>
      "(<[0-9A-F]{3}>)".r.findAllMatchIn(vt._2).map(_.group(0)).toList zip vals
    }.map{vals => println(s"vals: $vals");vals.foldLeft(DateTime.now){(dt, t) => t._1 match {
      case "<10A>" => dt.withYear(t._2.toInt)
      case "<10B>" => dt.withYear(t._2.toInt)
      case "<10C>" => dt.withYear(t._2.toInt)
      case "<10D>" => dt.withMonth(t._2.toInt)
      case "<10E>" => dt.withMonth(t._2.toInt)
      case "<10F>" => dt.withDayOfMonth(t._2.toInt)
      case "<111>" => dt.withDayOfMonth(t._2.toInt)
      case "<112>" => dt.withHour(t._2.toInt)
      case "<113>" => dt.withHour(t._2.toInt)
      case "<114>" => dt.withMinute(t._2.toInt)
      case "<115>" => dt.withMinute(t._2.toInt)
      case "<116>" => dt.withSecond(t._2.toInt)
      case "<117>" => dt.withSecond(t._2.toInt)
      case "<118>" => dt.withMillis(t._2.toInt)
      case "<119>" => dt.withMillis(t._2.toInt)
      case "<11A>" => dt.withMillis(t._2.toInt)
      case "<11B>" => dt.withOffset(Offset(t._2))
      case _ => dt
    }}}
  }.get

  def parseDateTime(format: String, value: String): DateTime = parse(format, value)

  def parseDate(format: String, value: String): Date = parse(format, value).toDate

  def parseTime(format: String, value: String): Time = parse(format, value).toTime

}
