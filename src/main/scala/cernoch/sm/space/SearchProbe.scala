package cernoch.sm.space

class SearchProbe[-S,-R] {

  def searchStarted() {}
  def searchStopped() {}
  def searchIteration
  (draughts: Iterable[(S,R)], breedins: Iterable[S]) {}

  def stateGenerated(state: S, from: S) {}
  def statesGenerated(states: Iterable[S]) {}

  def stateDropped(state: S, result:R) {}

  def evaluating(state: S) {}
  def evaluated(state: S, result: R) {}

  def draughtsGoingToBeSorted
  ( oldDraughts: Iterable[(S,R)],
    candidates: Iterable[(S,R)]) {}
  def draughtsHaveBeenSorted
  (newDraughts: Iterable[(S,R)]) {}

  def bestUpdated (state: S, result: R) {}

  def resettingBeamCounter(resettingState: S) {}
  def consideringStop(consecutiveNonImprovements: Int) {}
}
