package cernoch.sm.secret.transaction

import cernoch._
import scalogic._
import sm.space.SearchProbe
import sm.sql._
import tools.Labeler

/**
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
object BumpHunting {

  val domains = Domains all
  val schema = new Schema

  def main(args: Array[String])
  : Unit
  = {

    val sc = new Connect().toMySQL
    val ss = new SqlStorage(sc, List(schema.starter)).open
    val st = new Starter(schema.starter)

    val bl = WekaBridge(st,ss)
    println("Baseline accuracy = " +
      math.round(WekaBridge.classify(bl)) + "%")

    val ch = schema.others.toSet[Btom[FFT]]
    val nb = new NaiveBayesSearch(st,ch,ss,bl) {
      maxConsNonImp = 1
      beamWidth = 3
    }
    nb.probe = new SearchProbe
      [Horn[HeadAtom,Set[Atom[FFT]]],Out]() {
      override def searchIteration
      ( draughts: Iterable[(Horn[HeadAtom,Set[Atom[FFT]]],Out)],
        breedins: Iterable[Horn[HeadAtom,Set[Atom[FFT]]]] ) {
        println("==================================")
        println("     STARTING A NEW ITERATION   ")
        println("==================================")
      }

      override def statesGenerated
      (states: Iterable[Horn[HeadAtom,Set[Atom[FFT]]]]) {
        println("Generated " + states.size + " new clauses.")
      }

      override def bestUpdated
      (state: Horn[HeadAtom,Set[Atom[FFT]]], result: Out) {
        val n = Labeler.alphabet[Var]
        println(">>>>> In this iteration, a new optimum was found <<<<<")
        println("SCORE = " + result.acc + "% using "
          + result.agg + "(" + result.war.toString(n) + ").")
        println("STATE = " + state.toString(n))
      }

    }
     println(nb.call().head)
  }
}
