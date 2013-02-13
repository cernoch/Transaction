package cernoch.sm.secret.transaction

import cernoch.scalogic._


/**
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class Starter(val btom: Btom[FFT]) {

  val exVar = btom.args.view
    .filter{_.isInstanceOf[Var]}
    .find(_.dom == Domains.ex).get
    .asInstanceOf[Var]

  val clVar = btom.args.view
    .filter{_.isInstanceOf[Var]}
    .find(_.dom == Domains.cl).get
    .asInstanceOf[Var]

  val dtVar = btom.args.view
    .filter{_.isInstanceOf[Var]}
    .find(_.dom == Domains.dt).get
    .asInstanceOf[Var]

  val valVars = btom.args
    .filter{_.isInstanceOf[Var]}
    .filter(_ != exVar)
    .filter(_ != clVar)
    .asInstanceOf[List[Var]]

  val q = new Horn(
    new HeadAtom(exVar, clVar, dtVar, valVars),
    Set[Atom[FFT]](btom)
  )
}
