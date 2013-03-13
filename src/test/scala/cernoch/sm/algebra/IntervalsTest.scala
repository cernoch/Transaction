package cernoch.sm.algebra

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import cernoch.sm.algebra.Intervals._
import cernoch.scalistics.interval._


@RunWith(classOf[JUnitRunner])
class IntervalsTest extends Specification {
	import Basis._

  "Intervals intersection" should {

    "find all values" in {
      val i1 = Interval.openClosed(1,10)
      val i2 = Interval.openClosed(2,4)
      val i3 = Interval.openClosed(5,12)


      Intervals.intersections(List(List(i1), List(i2,i3))).toSet must_==
        Set(i2, Interval.openClosed(5,10))
    }
  }



  "Touching intervals" should {

    "succeed if really touching" in {
      val i1 = Interval.openClosed(1,10)
      val i2 = Interval.openClosed(10,20)

      touching(List(i1,i2)) && !touching(List(i2,i1))
    }
  }
}