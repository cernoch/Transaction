package cernoch.sm.secret.transaction

import cernoch.scalogic.{Atom, Var}

class HeadAtom(
    val exVar: Var,
    val clVar: Var,
    val baseDate: Var,
    val histVars: List[Var])
  extends Atom[Var](
    "head",
    exVar :: clVar :: histVars
  ) {

  def addOutVars
  (i: Iterable[Var])
  = new HeadAtom(
    exVar, clVar, baseDate,
    histVars ++ i
  )
}
