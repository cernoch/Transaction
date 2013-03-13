package cernoch.sm.space

import scala.math._
import cernoch.scalogic.{Term, Atom, Horn}

/**
 * Beam search algorithm with caching non-productive results
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
abstract class BeamSearch[State,Result]
  extends BestFirstSearch[State,Result] {

  var beamWidth = 5
  var maxConsNonImp = 10

  private var lastTime = 0
  private var lastBest : Option[State] = None



  override def call() = {
    lastTime = 0
    lastBest = None
    super.call()
  }



  def shallWeHalt
    (states: Iterable[(State, Result)])
  = {
    val best = states.isEmpty match {
      case true => None
      case false => Some(states.head._1)
    }

    if (best == lastBest) {
      lastTime = lastTime + 1
      probe.consideringStop(lastTime)
      lastTime >= maxConsNonImp

    } else {
      if (!states.isEmpty) {
        probe.resettingBeamCounter(states.head._1)
        lastBest = Some(states.head._1)
        lastTime = 0
      }
      false
    }
  }



  private def cropByBeamWidth
  (collection: Iterable[(State,Result)])
  = collection.zip(Stream from 1)
    .filter{ case ((s,r),i) => i <= beamWidth match {
      case false => false
      case true => { probe.stateDropped(s,r); true }
    }}.map{ case (k,v) => k }

  override def cropAndSort
    ( old: Iterable[(State,Result)],
      neu: Iterable[(State,Result)])
  = cropByBeamWidth(onlySort(old,neu))
  
  def onlySort
    ( old: Iterable[(State,Result)],
      neu: Iterable[(State,Result)])
  : Iterable[(State,Result)]
}