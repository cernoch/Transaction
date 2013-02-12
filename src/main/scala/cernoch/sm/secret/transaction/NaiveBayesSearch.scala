package cernoch.sm.secret.transaction

import cernoch._
import scalogic._
import scalistics._
import sm.algebra.aggregators._
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
   vychozi: Instances)
  extends ClauseBeam[(Var,String,Double)](st, modes) {

  def stateResult
  (state: Horn[HeadAtom, Set[Atom[FFT]]])
  = {

    println("QUERY: " + state)
    val noveHodnoty =
      for ( ((war,agg), map) <- execQuery(state) )
        yield {
        println("RESULT("+war+","+agg+"): " + map)
        (state, (war, agg, vychozi.enrich(war.dom, map).classify))
        }

    sortByResults(Array(), noveHodnoty).head._2
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
    val collector = new Labeler(f = (v: Val[_]) => {
      val aggCat = state.head.histVars.view
        .filter{_.dom.isInstanceOf[CatDom]}
        .map{_ -> mkCatAggss}
        .toMap

      val aggNum = state.head.histVars.view
        .filter{_.dom.isInstanceOf[NumDom]}
        .map{_ -> mkNumAggss}
        .toMap

      val aggDec = state.head.histVars.view
        .filter{_.dom.isInstanceOf[DecDom]}
        .map{_ -> mkDecAggss}
        .toMap

      (aggCat, aggNum, aggDec)
    })

    // Execute query and fill the aggregators
    for (mapa <- storage.query(state);
         (aggCat, aggNum, aggDec)
          = collector(mapa(state.head.exVar))) {

      for ((war,aggs) <- aggCat; wal = mapa(war)
           if wal != null; agg <- aggs)
        agg += wal.asInstanceOf[Cat].get

      for ((war,aggs) <- aggNum; wal = mapa(war)
           if wal != null; agg <- aggs)
        agg += wal.asInstanceOf[Num].get

      for ((war,aggs) <- aggDec; wal = mapa(war)
           if wal != null; agg <- aggs)
        agg += wal.asInstanceOf[Dec].get
    }

    val retVal = for (
      (example,varMapsInTuple) <- collector.map.toList;
      (ac,an,ad) = varMapsInTuple;
      varMapFromList <- Iterable(ac,an,ad);
      (war, aggregators) <- varMapFromList;
      aggregator <- aggregators;
      aggValue = aggregator()
      if aggValue.isDefined
    ) yield {
      val exampleId = example.asInstanceOf[Num].get.toInt
      ((war, aggregator.name), (exampleId, aggValue.get.toDouble))
    }

    retVal.groupBy{_._1}.mapValues{_.map{_._2}.toMap}
  }

  private implicit def bigInt2Dec(i: BigInt) = BigDecimal(i)

  private def mkDecAggss
  : List[Aggregator[BigDecimal,BigDecimal]]
  = List(
    MIN.forBigDecimal,
    MAX.forBigDecimal,
    SUM.forBigDecimal,
    MEAN.forBigDecimal,
    MEDIAN.forBigDecimal,
    VARIANCE.forBigDecimal
  )


  private def mkNumAggss
  : List[Aggregator[BigInt,BigInt]]
  = List(
    MIN.forBigInt,
    MAX.forBigInt,
    SUM.forBigInt,
    MODE.forBigInt,
    MEDIAN.forBigInt
  )



  private def mkCatAggss
  : List[Aggregator[Any,BigInt]]
  = List(COUNT.usingBigInt)



  /**
   * Returns a percentage from a nominator and denominator
   */
  private def percentage
  (nom: Int, denom: Int)
  = denom match {
    case 0 => "?%"
    case _ => (nom * 100 / denom) + "%"
  }

  /**
   * Given the number of examples
   * with and without a value,
   * do we have enough data?
   */
  def hasEnough
  (okay:Int, total:Int)
  = (total * 9 / 10) < okay

  private def byBestResult
  = (Ordering by {x: (Int,String,Hist[Double],Double) => x._4})

}
