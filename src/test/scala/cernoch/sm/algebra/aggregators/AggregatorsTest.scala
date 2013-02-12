package cernoch.sm.algebra.aggregators

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AggregatorsTest extends Specification {

  "SQL storage" should {
     "import data without an exception" in {
      true
    }
  }


  "Count" should {
    "count number of values" in { COUNT[Int](1,1,2) must_== 3 }
    "give zero on empty seq" in { COUNT[Int]() must_== 0 }
  }

  "Sum" should {
    "count number of values" in { SUM(1,1,2) must_== 4 }
    "give zero on empty seq" in { SUM[Int]() must_== 0 }
  }

  "Min" should {
    "pick the smallest value" in { MIN(3,2,4,5) must_== Some(2) }
    "return None on empty" in { MIN.forInt() must_== None }
    "work with BigInt" in { MIN(BigInt(10), BigInt(15)) must_== Some(BigInt(10)) }
  }

  "Max" should {
    "pick the largest value" in { MAX(3,2,4,5) must_== Some(5) }
    "return None on empty" in { MAX.forInt() must_== None }
    "work with BigInt" in { MAX(BigInt(10), BigInt(15)) must_== Some(BigInt(15)) }
  }

  "Mean" should {
    "pick the arithmetic mean" in { MEAN[BigDecimal](1,2,8,9,5) must_== BigDecimal(5) }
    "return None on empty" in { MEAN.forDouble() must_== None }
    "work with really large numbers" in {
      val aggregator = MEAN.forDoubleExact
      val iterStep = Double.MaxValue / 100;
      {
        var value = 0d
        while (value < Double.MaxValue) {
          aggregator += value
          value = value + iterStep
        }
      }
      val target = Double.MaxValue / 2
      math.abs(target - aggregator().get) < iterStep
    }
  }

  "Median" should {
    "pick the middle value" in { MEDIAN.onOrdered(1,2,3,4,5) must_== Some(3) }
    "ignore the ordering" in { MEDIAN.onOrdered(1,5,4,3,2) must_== Some(3) }

    "pick the middle in multi-modal distributions" in {
      MEDIAN.onOrdered(1,1,1,2,3,3,3) must_== Some(2)
    }
    "return None when there are two middle values" in {
      MEDIAN.onOrdered(1,2,3,4,5,6) must_== None
    }
    "return the rounded division when using Integers" in {
      ( MEDIAN.forInt ++ List(2,6) ).apply must_== Some(4)
    }
    "compute the average on fractional values" in {
      MEDIAN[BigDecimal](1,2,3,4,5,6) must_== Some(BigDecimal(7) / 2)
    }
  }

  "Mode" should {
    "pick the peak" in { MODE(1,2,2,3,3,3) must_== Some(3) }

    "give None in presence of equally high modes" in {
      MODE(1,1,1,3,3,3) must_== None
    }
  }

  "Variance" should {
    "return the correct value" in {
      VARIANCE[BigDecimal](1,3,5) must_== Some(BigDecimal(8))
    }

    "be invariant to a shift" in {
      VARIANCE[BigDecimal](2,4,6) must_== Some(BigDecimal(8))
    }

    "compute standard deviation" in {
      val target = math.sqrt(8)
      val agg = VARIANCE.standardDeviation ++ List(1,5)
      math.abs(target - agg().get) < 0.0001
    }
  }
}