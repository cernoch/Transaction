package cernoch.sm.secret.transaction

import cernoch._
import scalogic._
import sm.space.{Instant, BeamSearch, AddAtom, Generator}
import sm.space.Generator.instantiateHornBody

/**
 *
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
abstract class ClauseBeam[Result](st: Starter, mode: Set[Btom[FFT]])
  extends BeamSearch[ Horn[HeadAtom,Set[Atom[FFT]]], Result ] {

  /**
   * Initial state is an empty horn clause without a head
   */
  def sourceState = st.q

  /**
   * Adds new atoms to the horn clause based on the language bias
   */
  protected def addNewAtoms
  (orig: Horn[HeadAtom,Set[Atom[FFT]]])
  = Generator.addAtomToHorn(mode)(orig)

  protected def supported(v: Var)
  = !v.dom.isKey && WekaBridge.supported(v.dom)


  /**
   * Replaces the head in each refined clause
   */
  protected def replaceHead
  (addRefments: Iterable[AddAtom[Horn[HeadAtom,Set[Atom[FFT]]]]])
  = for (r <- addRefments)
  yield
    new AddAtom[Horn[HeadAtom,Set[Atom[FFT]]]]() {
      def added = r.added
      def old = r.old
      def neu = new Horn(
        r.old.head.addOutVars(r.added.variables.view // Only interested in variables...
          .filter{supported} // ... that satisify the user-specified criteria
          .filter{_.dom != r.old.head.exVar.dom} // ... not= example number (once is enough)
          .filter{_.dom != r.old.head.clVar.dom} // ... not= the class (classification would be trivial)
        ),
        r.neu.bodyAtoms )
    }

  /**
   * Instantiates variables in the newly added atom
   *
   * <p>Variables in the head of the clause is omitted.</p>
   */
  protected def instantiate
  (addedAtoms: Iterable[AddAtom[Horn[HeadAtom,Set[Atom[FFT]]]]])
  = for (
    ra <- addedAtoms;
    ri <- instantiateHornBody(ra.neu,
      ra.added.variables filterNot (
        ra.neu.head.variables contains
      ) // This also excludes "ex" and "cl"
    ) )
  yield new AddAtom[Horn[HeadAtom,Set[Atom[FFT]]]]
    with Instant[Horn[HeadAtom,Set[Atom[FFT]]]] {
      def old = ra.old
      def added = ra.added
      def oldVar = ri.oldVar
      def neuVal = ri.neuVal
      def neu = ri.neu
    }

  def descendants
  (clause: Horn[HeadAtom, Set[Atom[FFT]]])
  = {
    val expand = addNewAtoms(clause)
    val insted = instantiate(expand)

    (replaceHead(expand) ++ replaceHead(insted)).map{_.neu}
  }
}
