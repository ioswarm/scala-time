package de.ioswarm.time

import java.time.{Instant, ZoneId}

import org.scalatest.{Matchers, WordSpec}

class DateSpec extends WordSpec with Matchers {
  import concurrent.duration._

  val ldt = Instant.now().atZone(ZoneId.of("UTC")).toLocalDate
  val now = Date()
  "A Date() Instance" should {
    s"equal current UTC based date $ldt" in {
      now.year should be(ldt.getYear)
      now.month should be(ldt.getMonth.getValue)
      now.dayOfMonth should be(ldt.getDayOfMonth)
    }
  }

  List(
    "A Date(2018, 6, 12) Instance" -> Date(2018, 6, 12)
    , "A Date(\"2018-6-12\") Instance" -> Date("2018-06-12")
  ) foreach { tu =>
    tu._1 must {
      "year equal 2018" in {
        tu._2.year should be(2018)
      }
      "month equal 6" in {
        tu._2.month should be(6)
      }
      "dayOfMonth equal 12" in {
        tu._2.dayOfMonth should be(12)
      }
      "print text '2018-06-12'" in {
        tu._2.toText should be("2018-06-12")
      }
      "equal same date" in {
        tu._2 shouldEqual Date(2018,6,12)
      }
    }
  }

  "A date 2012-08-25" must {
    val d = Date(2012, 8, 25)
    "dayOfYear equal to 238" in {
      d.dayOfYear should be(238)
    }
    "dayOfWeek equal to 6" in {
      d.dayOfWeek should be(6)
    }
    "calendarWeek equal to 34" in {
      d.calendarWeek should be(34)
    }
    "equal same date when add 1 millisecond" in {
      (d + 1) shouldEqual d
      (d plus 1) shouldEqual d
    }
    "equal same date when add Duration 1.second" in {
      (d + 1.second) shouldEqual d
      (d plus 1.second) shouldEqual d
    }
    "equal same date when sub 1 millisecond" in {
      (d - 1) shouldEqual d
      (d minus 1) shouldEqual d
    }
    "equal same date when sub Duration 1.second" in {
      (d - 1.second) shouldEqual d
      (d minus 1.second) shouldEqual d
    }
    "equal '2018-08-26' when add 1 day" in {
      val x = Date(2012,8,26)
      (d + DAY_TO_MILLIS) shouldEqual x
      (d + 1.day) shouldEqual x
    }
    "equal same date when add 1 day minus 1 millisecond" in {
      (d + (DAY_TO_MILLIS-1l)) shouldEqual d
      (d + (1.day - 1.milli)) shouldEqual d
    }
    "equal '2012-08-30' when add 5 days" in {
      (d plusDays 5) shouldEqual Date(2012,8,30)
    }
    "equal '2012-08-20' when sub 5 days" in {
      (d minusDays 5) shouldEqual Date(2012, 8, 20)
    }
    "equal '2013-08-25' when add 365 days" in {
      (d plusDays 365) shouldEqual Date(2013,8,25)
    }
    "equal '2011-08-26' when sub 365 days" in {
      (d minusDays 365) shouldEqual Date(2011, 8, 26)
    }
    "equal '2013-08-25' when add 12 months" in {
      (d plusMonths 12) shouldEqual Date(2013,8,25)
    }
    "equal '2011-08-25' when sub 12 months" in {
      (d minusMonths 12) shouldEqual Date(2011,8,25)
    }
    "equal '2022-08-25' when add 10 years" in {
      (d plusYears 10) shouldEqual Date(2022,8,25)
    }
    "equal '2002-08-25' when sub 10 years" in {
      (d minusYears 10) shouldEqual Date(2002,8,25)
    }
  }

  "A Date(-200, 12, 31)" must {
    "throw an IllegalArgumentException" in {
      intercept[IllegalArgumentException](Date(-200,12,31))
    }
  }

  "A Date(2000, 13, 31)" must {
    "throw an IllegalArgumentException" in {
      intercept[IllegalArgumentException](Date(2000,13,31))
    }
  }

  "A Date(2011, 2, 29)" must {
    "throw an IllegalArgumentException" in {
      intercept[IllegalArgumentException](Date(2011,2,29))
    }
  }

}