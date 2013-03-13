package cernoch.sm.space

import grizzled.slf4j.Logging

class VerboseProbe[-S,-R]
	extends SearchProbe[S,R]
	with Logging {

  override def searchStarted() {
    info("Search started")
  }
  override def searchStopped() {
    info("Search stopped")
  }
  override def searchIteration
  (draughts: Iterable[(S,R)], breedins: Iterable[S]) {
    info("New iteration with " + draughts.size +
      " draughts and " + breedins.size + " breedins.")
  }

  override def stateGenerated(state: S, from: S) {
    debug("Gerenated new state: " + state)
  }
  override def statesGenerated(i: Iterable[S]) {
    trace("All new states generated.")
  }

  override def stateDropped(state: S, result:R) {
    trace("State has been dropped out: "+ state)
  }

  override def evaluating(state: S) {
    trace("About to evaluate: "+ state)
  }
  override def evaluated(state: S, result: R) {
    debug("State "+ state + " evaluated as " + result)
  }

  override def draughtsGoingToBeSorted
  ( oldDraughts: Iterable[(S,R)],
    candidates: Iterable[(S,R)]) {
		trace("About to sort draughts.\nold=" + oldDraughts + "\nnew="+candidates)
	}

  override def draughtsHaveBeenSorted
  (newDraughts: Iterable[(S,R)]) {
		trace("Draughts have been sorted.\n" + newDraughts)
	}

  override def bestUpdated (state: S, result: R) {
    info("New best state found! " + state)
  }

  override def resettingBeamCounter(resettingState: S) {}
  override def consideringStop(c: Int) {
    debug("Number of consecutive non-improvements: " + c)
  }
}
