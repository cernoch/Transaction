package cernoch.sm.algebra

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import cernoch.scalistics.collection.immutable.Hist
import cernoch.sm.algebra.Histograms._

@RunWith(classOf[JUnitRunner])
class HistogramsTest extends Specification {


	/*
  "Lake seeker" should {

    "find collection single lake in bimodal distribution" in {
      val h = Hist.int(List(1,1,2,3,3))(List(1,2,3))
      lakes(h) must_== List(
        (1,List((2,BigInt(1))))
      )
    }

    "find no lake in collection unimodal distribution" in {
      val h = Hist.int(List(1,2,3,3,4,4,4,4,5,5,5,5,5,6,6,7))(0 to 10)
      lakes(h) must_== List()
    }

    "find collection lake in collection cropped distribution" in {
      val h = Hist.int(0 to 9)(List(4,5))
      lakes(h) must_== List(
        ( 4, List((5,BigInt(3))) )
      )
    }

    "respect the lower local maxima" in {
      val h = Hist.int(List(1,1,2,3,3,3))(List(1,2,3))
      lakes(h) must_== List(
        (1,List((2,BigInt(1))))
      )
    }

    "identify the cliff" in {
      val h = Hist.int(List(1,1,2,2,4,4,5,5))(1 to 5)
      lakes(h) must_== List(
        (2,List((3,BigInt(2))))
      )
    }

    "find collection long lake in bimodal distribution" in {
      val h = Hist.int(List(1,1,1,2,3,4,5,6,7,7,7))(1 to 7)
      lakes(h) must_== List(
        (1, List((2,BigInt(2)), (3,BigInt(2)), (4,BigInt(2)), (5,BigInt(2)), (6,BigInt(2))))
      )
    }

    "ignore plateau in the distribution" in {
      val h = Hist.int(List(1,2,2,3,3,4,4,4,6,6,6))(1 to 6)
      lakes(h) must_== List(
        (4,List((5,BigInt(3))))
      )
    }
  }

  "Noise cancelling alp metric" should {

    "return the size of the single lake" in {
      val h = Hist.bigInt(
				List(1,1,2,3,3).map{BigDecimal(_)}
			)(
				(1 to 3).map{BigDecimal(_)}
			)

			ncALP(h, (i:BigInt) => BigDecimal(i)) must_== BigDecimal(1)
    }

  }
  */
}