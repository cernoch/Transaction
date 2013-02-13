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
class NaiveBayesSearch
  (st: Starter,
   modes: Set[Btom[FFT]],
   storage: QueryExecutor,
   vychozi: WekaBridge[Val[_]])
  extends ClauseBeam[(Var,String,Double)](st, modes) {

  def stateResult
  (state: Horn[HeadAtom, Set[Atom[FFT]]])
  = {
    var bestScore = 0.0
    var bestState: Option[(Var,String,Double)] = None

    val varNames = Labeler.alphabet[Var]
    println("----- Executing a new query -----")
    println(state.toShort(varNames))

    execQuery(state).par.foreach{
      case ((war,agg), exampleMap) => {
        val newAttr = Var(DecDom(war.dom.name))
        val enriched = vychozi.enrich(newAttr, exampleMap)
        val score = WekaBridge.classify(enriched)
        state.synchronized {
          if (bestScore < score) {
            bestScore = score
            bestState = Some((war, agg, score))
          }
        }
      }
    }

    if (bestState.isEmpty) throw new
        TooNew("No aggregable variable in the query.")

    println("Best accuracy " + bestState.get._3 +
      "% achieved using " + bestState.get._2.toUpperCase +
      " on variable " + bestState.get._1.toString(varNames) + ".")
    bestState.get
  }

  def sortByResults
  (old:    Array[(Horn[HeadAtom, Set[Atom[FFT]]], (Var,String,Double))],
   neu: Iterable[(Horn[HeadAtom, Set[Atom[FFT]]], (Var,String,Double))])
  = (old ++ neu) sortBy {- _._2._3}



  /**
   * Execute the query.
   */
  def execQuery(state: Horn[HeadAtom, Set[Atom[FFT]]]) = {
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

    // Start printing the status every 30s
    var results = BigInt(0)
    val started = System.currentTimeMillis()

    val writer = new Thread() {
      override def run() {
        try {
          while (true) {
            Thread.sleep(10000)
            print("... in " +
              (System.currentTimeMillis() - started) / 1000 +
              "s done "+results + " results using " + Tools.usedMem +
              "MB mem")
          }
        } catch {
          case _:InterruptedException =>
        }
      }
    }
    writer.start()

    // Execute query and fill the aggregators
    storage.query(state, resMap => {

      if (results == 0) {
        val delta = (System.currentTimeMillis().toDouble - started) / 1000
        print("... query evaluated in " + delta + "s")
      }

      for (
        (war, agg) <- collector(resMap(state.head.exVar));
        wal = resMap(war) if wal.get != null
      ) agg match {
        case a:AggCat => a += wal.asInstanceOf[Cat].get
        case a:AggDec => a += wal.asInstanceOf[Dec].get
        case a:AggNum => a += wal.asInstanceOf[Num].get
      }
      results = results + 1
    })
    writer.interrupt()
    println("... total number of " + results + " records for " +
      collector.map.size + " examples aggregated in " +
      (System.currentTimeMillis().toDouble -
        started.toDouble) / 1000 + "s.")

    if (results == 0) throw new
      TooOld("Query did not produce a single result")

    /*
    collector.map.foreach{case (k,v) => {
      println(" ===== " + k + " ===== ")
      v.foreach{case (v,f) => println(v + " => " + f())}
    }}
    */

    val retVal = for (
      (example,varMapsInTuple) <- collector.map.toList;
      (war, aggregators) <- varMapsInTuple;
      (aggName, value) <- aggregators()
    ) yield ((war, aggName), (example, value))

    retVal.groupBy{_._1}.mapValues{_.map{_._2}.toMap}
  }

  private implicit def bigInt2Dec(i: BigInt) = BigDecimal(i)
}
