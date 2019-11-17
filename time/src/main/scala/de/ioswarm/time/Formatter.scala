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
      })
      .foldLeft(format){(format, t) => format.replaceAll(t._1, t._2)}
      .replace("Z", temporal.offset.toText)
  }

}
