package cernoch.sm.space

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner


@RunWith(classOf[JUnitRunner])
class BeamSearchTest extends Specification {

  val graf = Graph[Int] +=
    1 -> Set(2) +=
    2 -> Set(3) +=
    3 -> Set(4) +=
    4 -> Set(5) +=
    5 -> Set(6) +=
    6 -> Set(7) +=
    7 -> Set(8) +=
    8 -> Set(9)

  val bfs = new BeamSearch[Int,Int]() {
    beamWidth = 3
    maxConsNonImp = 2

    def sourceState: Int = 1
    def stateResult(state: Int): Int = state

    def descendants(state: Int): Iterable[Int]
    = graf.g.get(state).getOrElse(Set())

    def onlySort(old: Iterable[(Int, Int)], neu: Iterable[(Int, Int)])
    = (old ++ neu).toList.sortWith((a,b) => a._2 > b._2)
  }

  "BestFirstSearch" should {
    "terminate and find all states" in {
      bfs.call().head._1 must_==  9
    }
  }
}