package cernoch.sm.secret.transaction

import cernoch._
import scalogic._
import scalistics._
import sm.algebra.aggregators._
import sm.space.{TooOld, TooNew}
import sm.sql.QueryExecutor
import tools.Labeler
import weka.core.Instances
import WekaBridge._

/**
 * Bump-hunting algorithm via ncALP metric
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */

case class Out(val war: Var, val agg: String,
  val dat: WekaBridge[Val[_]], val acc: Double) {}


class NaiveBayesSearch
  (st: Starter,
   modes: Set[Btom[FFT]],
   storage: QueryExecutor,
   vychozi: WekaBridge[Val[_]])
  extends ClauseBeam[Out](st, modes) {

  Class.forName("org.postgresql.util.PSQLException")

  def stateResult
  (state: Horn[HeadAtom, Set[Atom[FFT]]])
  = {
    var best: Out = null

    val varNames = Labeler.alphabet[Var]
    println("----- Executing a new query -----")
    println(state.toShort(varNames))

    // Prepare for OutOfMemoryException
    val tooNew = new TooNew(
      "Query produces too much results." +
        " Must be specialized")

    try {
      execQuery(state).par.foreach {
        case ((war,agg), exampleMap) => {
          val newAttr = Var(DecDom(war.dom.name))
          val enriched = vychozi.enrich(newAttr, exampleMap)
          val acc = WekaBridge.classify(enriched)
          state.synchronized {
            if (best == null || best.acc < acc)
              best = new Out(war, agg, enriched, acc)
          }
        }
      }
    } catch {
      case _:OutOfMemoryError => throw tooNew
    }

    if (best == null) throw new
        TooNew("No aggregable variable in the query.")

    println("Best accuracy " + best.acc +
      "% achieved using " + best.agg +
      " on variable " + best.war.toString(varNames) + ".")
    best
  }

  def onlySort
  ( old: Iterable[(Horn[HeadAtom, Set[Atom[FFT]]], Out)],
    neu: Iterable[(Horn[HeadAtom, Set[Atom[FFT]]], Out)])
  = (old.view ++ neu).toArray.sortBy{- _._2.acc}



  /**
   * Execute the query.
   */
  def execQuery(state: Horn[HeadAtom, Set[Atom[FFT]]]) = {

    // Start printing the status every 10s
    var results = BigInt(0)
    val started = System.currentTimeMillis()
    val writer = new Thread("StatusWriter") {
      override def run() {
        try {
          while (true) {
            Thread.sleep(10000)
            print("... in " +
              (System.currentTimeMillis() - started) / 1000 +
              "s "+results + " results done using " + Tools.usedMem +
              "MB mem")
          }
        } catch {
          case _:OutOfMemoryError =>
          case _:InterruptedException =>
        }
      }
    }

    try {
      writer.start()

      // Create a triplet of aggregators for each example
      val collector = new Labeler((v: Val[_]) => {
        state.head.histVars.view
          .filterNot{_.dom.isKey}
          .map(war => { war -> (war.dom match {
          case CatDom(_,_,_) => new AggCat()
          case NumDom(_,_) => new AggNum()
          case DecDom(_) => new AggDec()
        })}).toList
      })

      // Execute query and fill the aggregators
      storage.query(state, resMap => {

        if (results == 0) {
          val delta = (System.currentTimeMillis().toDouble - started) / 1000
          print("... query evaluated in " + delta + "s")
        }

        for (
          (war, agg) <- collector(resMap(state.head.exVar)).view
            .filter{case (war,_) => resMap(war).get != null}.par
        ) agg match {
          case a:AggCat => a += resMap(war).asInstanceOf[Cat].get
          case a:AggDec => a += resMap(war).asInstanceOf[Dec].get
          case a:AggNum => a += resMap(war).asInstanceOf[Num].get
        }
        results = results + 1
      })

      println("... total number of " + results + " records for " +
        collector.map.size + " examples aggregated in " +
        (System.currentTimeMillis().toDouble -
          started.toDouble) / 1000 + "s and " +
        Tools.usedMem + "MB memory.")

      if (results == 0) throw new
        TooOld("Query did not produce a single result")
      val retVal = for (
        (example,varMapsInTuple) <- collector.map.toList;
        (war, aggregators) <- varMapsInTuple;
        (aggName, value) <- aggregators()
      ) yield ((war, aggName), (example, value))

      retVal.groupBy{_._1}.mapValues{_.map{_._2}.toMap}

    } finally {
      writer.interrupt()
    }
  }

  private implicit def bigInt2Dec(i: BigInt) = BigDecimal(i)
}
