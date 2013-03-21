package cernoch.sm.secret.transaction

import cernoch.scalogic._

object State {

	def apply
	(initSchema: Schema)
	= new State(
		initSchema = initSchema,
		clauseBody = Set(initSchema.atom),
		valuedVars = initSchema.other
			.filter{v => WekaBridge.supported(v.dom)}
	)
}

/**
 * State in the clause-space
 *
 * @param valuedVars Variables that can be aggregated
 * @param clauseBody Body of the clause
 */
case class State
	(initSchema: Schema,
	 valuedVars: List[Var],
	 clauseBody: Set[Atom])
{
	import initSchema._

	val head = new StampedAtom(
		ident, klass, stamp, valuedVars)

	val horn = Horn(head, clauseBody)
}

trait AddedAtom {

	val added: Atom
}

trait NumericInEq
	extends AddedAtom {

	val value: Val

	def relax: State

	def tighten: State
}
