package cernoch.sm.algebra.aggregators

import cernoch.sm.algebra.aggregators._

class AggNum extends (() => Map[String,Double]) {

  private implicit def bigInt2bigDec(l: BigInt) = BigDecimal(l)

  private var _cnt = BigInt(0)
  private var _suma = BigInt(0)

  private val _min = MIN.forBigInt
  private val _max = MAX.forBigInt
  private val _median = MEDIAN.forBigInt

  private def _mean = new MEAN(new SUM(BigDecimal(_suma)), new SUM(_cnt))

  def min = "MIN" -> _min.apply
  def max = "MAX" -> _max.apply
  def count = "COUNT" -> Some(_cnt)
  def mean = "MEAN" -> _mean.apply
  def median = "MEDIAN" -> _median.apply
  def mode = "MODE" -> new MODE(_median.map).apply

  def apply
  = List(min, max, count, mean, median, mode)
      .filter{case (_,o) => o.isDefined}.toMap
      .mapValues{_.get.toDouble}

  def +=(v: BigInt) = {
    _cnt = _cnt + 1
    _suma = _suma + v

    _min += v
    _max += v
    _median += v
  }
}
