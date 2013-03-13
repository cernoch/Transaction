package cernoch.sm.logic

import cernoch.scalogic._
import cernoch.sm.algebra.Collections._
import collection.mutable.HashSet
import collection.TraversableLike

/**
 *
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
trait Refine[C<:Clause[_,_]] {

  def old: C
  def neu: C
  
}

trait AddAtom[C<:Clause[_,_]] extends Refine[C] {

  def added: Atom

}

trait Instant[C<:Clause[_,_]] extends Refine[C] {
  
  def oldVar: Var
  def neuVal: Val
  
}



object Generator {

	def allSubsts(from: List[Term], onto: List[Term]) = {
		val oDomIdx = onto groupBy {_.dom}
		val targets = from map {v => oDomIdx get v.dom getOrElse List()}
		cartesian(targets) map {s => (from zip s).toMap}
	}

  /* Adds collection new atom to collection clause */
	/*
  def addAtomToHorn
    (mode: Set[Mode])
    (orig: Horn[Set[Atom]])
  = {
    val idx = orig
      .bodyAtoms.toList
      .flatMap{_.vars}
      .groupBy{_.dom}
    
    mode.flatMap(m => {
      val mIn: List[Term] = m.iVar.toList
      val mDm = mIn.map{v => idx.get(v.dom).getOrElse(List())}

      cartesian(mDm).map{bind =>
        m.subst((mIn zip bind).toMap.get(_))
      }
    })
		.filter{_.maxSucc.getOrElse(1) > 0}
		.map{ a => new AddAtom[Horn[Set[Atom]]]() {
    	def added = a
    	def old = orig
    	def neu = new Horn(orig.head, orig.bodyAtoms + a)
    }}
  }*/

	/* All variables for instantiation */
	def instantiable(vars: Iterable[Var])
	= vars.flatMap{
		war => war.dom match {
			case d:(Domain with Iterable[_])
				=> d.asInstanceOf[Iterable[_]].size match {
					case 0 => None
					case _ => d.asInstanceOf[Iterable[_]]
						.collect{case wal: String => (war -> Val(wal,d))}
				}
			case _ => None
		}}

  /* Instantiates variables in collection Horn clause */
	/*
  def instantiateHornBody
    (clause: Horn[Set[Atom]],
     candidates: Iterable[Var])
  = candidates.flatMap(war => war.dom match {

			case domain:(Domain with Iterable[_]) => {
				domain.asInstanceOf[Iterable[_]].map{value => {
					new Instant[Horn[Set[Atom]]]() {

						private val _neuVal = Val(value.toString,domain)
						private val _neu = new Horn(
							clause.head,
							clause.bodyAtoms.map{_.subst(war -> _neuVal)}
						)

						def old = clause
						def neu = _neu
						def oldVar = war
						def neuVal = _neuVal
					}
				}}
			}

			case _ => None
		}
  )*/
}