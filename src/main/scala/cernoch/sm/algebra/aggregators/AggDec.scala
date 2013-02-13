package cernoch.sm.algebra.aggregators

import cernoch.sm.algebra.aggregators._

class AggDec extends (() => Map[String,Double]) {

  private implicit def bigInt2bigDec(l: BigInt) = BigDecimal(l)

  private var _cnt = BigInt(0)
  private var _suma = BigDecimal(0)
  private var _sumSqr = BigDecimal(0)

  private val _min = MIN.forBigDecimal
  private val _max = MAX.forBigDecimal
  private val _median = MEDIAN.forBigDecimal

  private def _mean = new MEAN(new SUM(_suma), new SUM(_cnt))
  private def _variance = new VARIANCE(new SUM(_sumSqr), _mean)

  def min = ("MIN", _min.apply)
  def max = ("MAX", _max.apply)
  def count = ("COUNT", Some(_cnt))
  def mean = ("MEAN", _mean.apply)
  def median = ("MEDIAN", _median.apply)
  def variance = ("VARIANCE", _variance.apply)
  def stdDev = ("STDDEV", variance._2
    .filter{_ > Double.MaxValue}
    .map{_.toDouble}
    .map(math.sqrt))

  def apply = List(min, max, count, mean, median, variance)
      .map{case (name, agg) => name -> agg.map{_.toDouble}}
      .toMap.+(stdDev) // Add stdDev after converting to Double
      .filter{case (_,agg) => agg.isDefined}.mapValues{_.get}

  def +=(v: BigDecimal) = {
    _cnt = _cnt + 1
    _suma = _suma + v
    _sumSqr = _sumSqr + (v * v)

    _min += v
    _max += v
    _median += v
  }
}
