package cernoch.sm.space

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner


@RunWith(classOf[JUnitRunner])
class BestFirstSearchTest extends Specification {

  val graf = Graph[Int] +=
    1 -> Set(2,3) +=
    2 -> Set(3,4,5) +=
    4 -> Set(6)

  val bfs = new BestFirstSearch[Int,Int]() {
    def sourceState: Int = 1
    def stateResult(state: Int): Int = state

    def descendants(state: Int): Iterable[Int]
    = graf.g.get(state).getOrElse(Set())

    def shallWeHalt(s: Iterable[(Int, Int)])
    = s.toSet.contains((6,6))
  }

  "BestFirstSearch" should {
    "terminate and find all states" in {
      bfs.call().map{_._1}.toSet must_==  (1 to 6).toSet
    }
  }
}