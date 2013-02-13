package cernoch.sm.algebra.aggregators

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import math.round

@RunWith(classOf[JUnitRunner])
class AggregatorsTest2 extends Specification {

  implicit def double2matcher(d: Double) = new DoubleMatcher(d);
  class DoubleMatcher(d: Double) {
    def isRoughly(t: Double) = math.abs(d-t) < 0.1
  }

  "Cat aggregator" should {
    val a = new AggCat()
    a += "ahoj"
    a += "lidi"
    "count number of values" in { round(a()("COUNT")) must_== 2 }
  }

  "Num aggregator" should {
    val a = new AggNum()
    a += 3
    a += 4
    a += 6
    a += 6
    a += 6

    "min" in { round(a()("MIN")) must_== 3 }
    "max" in { round(a()("MAX")) must_== 6 }
    "count" in { round(a()("COUNT")) must_== 5 }
    "get mean" in { round(a()("MEAN")) must_== 5 }
    "get mode" in { round(a()("MODE")) must_== 6 }
  }

  "Dec aggregator" should {
    val a = new AggDec()
    a += 3
    a += 4
    a += 6
    a += 6
    a += 6

    "min" in { a()("MIN") isRoughly 3 }
    "max" in { a()("MAX") isRoughly 6 }
    "count" in { a()("COUNT") isRoughly 5 }
    "get mean" in { a()("MEAN") isRoughly 5 }
  }
}