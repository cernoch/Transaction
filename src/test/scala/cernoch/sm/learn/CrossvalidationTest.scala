package cernoch.sm.learn

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import Crossvalidation._

@RunWith(classOf[JUnitRunner])
class CrossvalidationTest extends Specification {

  "Leave one out crossvalidation" should {

    "be return only 1 testing instance in each fold" in {
      val x = List(1,2,3,4,5)
      val c = leaveOneOut(x)

      (c map {x map _} map {_ filter {_ == true} size}) must_== List(1,1,1,1,1)
    }
  }

  "N-fold crossvalidation" should {

    "Split the dataset into halves" in {
      val x = List(1,2,3,4,5,6)
      val c = nFold(2,x)

      (c map {x map _} map {_ filter {_ == true} size}) must_== List(3,3)
    }
  }
}