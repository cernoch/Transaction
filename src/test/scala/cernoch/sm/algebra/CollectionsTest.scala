package cernoch.sm.algebra

import Collections._

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CollectionsTest extends Specification {

	"Equal elements" should {

		class X {
			override def equals(a:Any) = a.isInstanceOf[X]
		}

		"be identified using the allEqual" in {
			allEqual(List(new X, new X, new X))
		}

		"be identified using the allEqual" in {
			! allEqual(List(new X, new X, "Ahoj"))
		}
	}



	"Carthesian product" should {

		"combine each element from each dimension" in {
			cartProd(
				List(
					List("Hello"),
					List("world!", "John,")
				)
			){_  + " " + _}.toSet must_==
				Set("Hello world!", "Hello John,")
		}
	}


	"Differentiate" should {

		"return an array 1 shorter than the given" in {
			differentiate(List(1,2,3,2,1)).length must_== 4
		}

		"compute the values correctly" in {
			differentiate(List(1,5,3)) must_== List(4,-2)
		}

		"succeed on collection list of length 1" in {
			differentiate(List(0)) must_== List()
		}
	}
}