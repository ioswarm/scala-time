package de.ioswarm.time

import org.scalatest.{Matchers, WordSpec}

class OffsetSpec extends WordSpec with Matchers {

  "An Offset()" should {
    val o = Offset()
    "have 0 millis" in {
      o.millis should be (0l)
    }
    "have 0 hours" in {
      o.hours should be(0)
    }
    "have 0 minutes" in {
      o.minutes should be(0)
    }
    "return 'Z' as text" in {
      o.toText should be("Z")
    }
  }

  /*"An Offset(-12, 30)" should {
    "throw an IllegalArgumentException" in {
      intercept[IllegalArgumentException](Offset(-12, 30))
    }
  }

  "An Offset(12, 30)" should {
    "throw an IllegalArgumentException" in {
      intercept[IllegalArgumentException](Offset(12, 30))
    }
  }*/

  "An Offset('-01:00')" should {
    val o = Offset("-01:00")

    "return '+00:10' when add '+01:10'" in {
      val ox = o + Offset(1,10)
      ox.hours should be(0)
      ox.minutes should be(10)
      ox.toText should be("+00:10")
    }

    "return '-10:30' when sub '+09:30'" in {
      val ox = o - Offset(9, 30)
      ox.hours should be (-10)
      ox.minutes should be (-30)
      ox.toText should be ("-10:30")
    }
  }

}