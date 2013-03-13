package cernoch.sm.secret.transaction

import cernoch.scalogic._

/**
 * State in the clause-space
 *
 * @param valuedVars Variables that can be aggregated
 * @param clauseBody Body of the clause
 */
class State(valuedVars: List[Var], clauseBody: Set[Atom]) {
	import State._

	val head = new StampedAtom(exId, klass, stamp, valuedVars)

	val horn = Horn(head, clauseBody)
}

trait AddedAtom {

	val added: Atom
}

trait NumericInEq extends AddedAtom {

	val value: Val

	def relax: State

	def tighten: State
}


object State {

	val exId = Schema.starter.atom.args.view
		.filter{_.isInstanceOf[Var]}
		.find(_.dom == Domains.ex).get
		.asInstanceOf[Var]

	val klass = Schema.starter.atom.args.view
		.filter{_.isInstanceOf[Var]}
		.find(_.dom == Domains.cl).get
		.asInstanceOf[Var]

	val stamp = Schema.starter.atom.args.view
		.filter{_.isInstanceOf[Var]}
		.find(_.dom == Domains.dt).get
		.asInstanceOf[Var]

	val others = Schema.starter.atom.args
		.filter{_.isInstanceOf[Var]}
		.filter(_ != exId)
		.filter(_ != klass)
		.filter(_ != stamp)
		.asInstanceOf[List[Var]]

	def apply()
	= new State(
		others.filter{war => WekaBridge.supported(war.dom)},
		Set(Schema.starter.atom)
	)
}