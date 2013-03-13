package cernoch.sm.secret.transaction

import cernoch.scalogic.{Atom, Var}

class ExAtom
	(val exId: Var,
	 val exProps: List[Var])
	extends Atom("", exId :: exProps)

class SuperAtom
	(exId: Var,
	 val klass: Var,
	 val attribs: List[Var])
	extends ExAtom(exId, klass :: attribs)

class StampedAtom
	(exId: Var,
	 klass: Var,
	 val stamp: Var,
	 val others: List[Var])
  extends SuperAtom(exId, klass, stamp :: others)
