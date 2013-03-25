package cernoch.sm.transaction

import cernoch.sm.space.SearchProbe
import cernoch.scalogic.tools.Labeler
import cernoch.scalogic.Var
import grizzled.slf4j.Logging

class LoggingProbe
	extends SearchProbe[State,Result]
	with Logging {

	override def searchIteration
	( draughts: Iterable[(State,Result)],
		breedins: Iterable[State] ) {
		info("STARTING A NEW ITERATION")
	}

	override def statesGenerated
	(states: Iterable[State]) {
		debug(s"Generated ${states.size} new clauses.")
	}

	override def bestUpdated
	(state: State, result: Result) {
		info(s"In this iteration, a new optimum was found: "
			+ LoggingProbe.stateScore(state,result) )
	}
}

object LoggingProbe {

	def stateScore(state: State, result: Result)
	= {
		val names  = Labeler.alphabet[Var]
		val clause = state.horn.toString(short=true,  names=names)
		val aggVar = result.war.toString(short=false, names=names)
		val score  = Result.Format.format(result.acc)
		s"+$score% using ${result.agg}($aggVar) on $clause."
	}
}
