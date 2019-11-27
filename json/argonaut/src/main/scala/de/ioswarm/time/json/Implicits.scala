package de.ioswarm.time.json

import argonaut._
import Argonaut._
import de.ioswarm.time.{Date, DateTime, Formatter, Time, dateFormat, dateTimeFormat, timeFormat}

trait Implicits {

  import java.util.{Date => UDate}
  import java.sql.{Date => SDate, Time => STime, Timestamp => STimestamp}
  import java.time.{Instant, LocalDate, LocalTime, LocalDateTime}
  import java.text.SimpleDateFormat
  import java.time.format.DateTimeFormatter


  implicit def _argonautEncodeInstant: EncodeJson[Instant] = EncodeJson{ i =>
    jString(i.toString)
  }
  implicit def _argonautDecodeInstant: DecodeJson[Instant] = implicitly[DecodeJson[String]].map{ s =>
    Instant.parse(s)
  }

  // Date
  implicit def _argonautEncodeDate: EncodeJson[Date] = EncodeJson{ d =>
    jString(Formatter.format(dateFormat, d))
  }
  implicit def _argonautDecodeDate: DecodeJson[Date] = implicitly[DecodeJson[String]].map{ s =>
    Formatter.parseDate(dateFormat, s)
  }

  implicit def _argonautEncodeUDate: EncodeJson[UDate] = EncodeJson{ d =>
    val sdf = new SimpleDateFormat(dateFormat)
    jString(sdf.format(d))
  }
  implicit def _argonautDecodeUDate: DecodeJson[UDate] = implicitly[DecodeJson[String]].map{ s =>
    val sdf = new SimpleDateFormat(dateFormat)
    sdf.parse(s)
  }

  implicit def _argonautEncodeSDate: EncodeJson[SDate] = EncodeJson{ d =>
    val sdf = new SimpleDateFormat(dateFormat)
    jString(sdf.format(d))
  }
  implicit def _argonautDecodeSDate: DecodeJson[SDate] = implicitly[DecodeJson[String]].map{s =>
    val sdf = new SimpleDateFormat(dateFormat)
    new SDate(sdf.parse(s).getTime)
  }

  implicit def _argonautEncodeLocalDate: EncodeJson[LocalDate] = EncodeJson{ d =>
    val fmt = DateTimeFormatter.ofPattern(dateFormat)
    jString(fmt.format(d))
  }
  implicit def _argonautDecodeLocalDate: DecodeJson[LocalDate] = implicitly[DecodeJson[String]].map{ s =>
    val fmt = DateTimeFormatter.ofPattern(dateFormat)
    LocalDate.parse(s, fmt)
  }


  // Time
  implicit def _argonautEncodeTime: EncodeJson[Time] = EncodeJson{ t =>
    jString(Formatter.format(timeFormat, t))
  }
  implicit def _argonautDecodeTime: DecodeJson[Time] = implicitly[DecodeJson[String]].map{ s =>
    Formatter.parseTime(timeFormat, s)
  }

  implicit def _argonautEncodeSTime: EncodeJson[STime] = EncodeJson{ t =>
    val sdf = new SimpleDateFormat(timeFormat)
    jString(sdf.format(t))
  }
  implicit def _argonautDecodeSTime: DecodeJson[STime] = implicitly[DecodeJson[String]].map{ s =>
    val sdf = new SimpleDateFormat(timeFormat)
    new STime(sdf.parse(s).getTime)
  }

  implicit def _argonautEncodeLocalTime: EncodeJson[LocalTime] = EncodeJson{t =>
    val fmt = DateTimeFormatter.ofPattern(timeFormat)
    jString(fmt.format(t))
  }
  implicit def _argonautDecodeLocalTime: DecodeJson[LocalTime] = implicitly[DecodeJson[String]].map{ s =>
    val fmt = DateTimeFormatter.ofPattern(timeFormat)
    LocalTime.parse(s, fmt)
  }


  // DateTime
  implicit def _argonautEncodeDateTime: EncodeJson[DateTime] = EncodeJson{ dt =>
    jString(Formatter.format(dateTimeFormat, dt))
  }
  implicit def _argonautDecodeDateTime: DecodeJson[DateTime] = implicitly[DecodeJson[String]].map{ s =>
    Formatter.parse(dateTimeFormat, s)
  }

  implicit def _argonautEncodeSTimestamp: EncodeJson[STimestamp] = EncodeJson{ t =>
    val sdf = new SimpleDateFormat(dateTimeFormat)
    jString(sdf.format(t))
  }
  implicit def _argonautDecodeSTimestamp: DecodeJson[STimestamp] = implicitly[DecodeJson[String]].map{ s =>
    val sdf = new SimpleDateFormat(dateTimeFormat)
    new STimestamp(sdf.parse(s).getTime)
  }

  implicit def _argonautEncodeLocalDateTime: EncodeJson[LocalDateTime] = EncodeJson{ dt =>
    val fmt = DateTimeFormatter.ofPattern(dateTimeFormat)
    jString(fmt.format(dt))
  }
  implicit def _argonautDecodeLocalDateTime: DecodeJson[LocalDateTime] = implicitly[DecodeJson[String]].map{ s =>
    val fmt = DateTimeFormatter.ofPattern(dateTimeFormat)
    LocalDateTime.parse(s, fmt)
  }

}
object Implicits extends Implicits