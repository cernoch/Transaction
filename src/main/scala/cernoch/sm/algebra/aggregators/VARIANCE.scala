package cernoch.sm.algebra.aggregators

class VARIANCE[T,N]
    ( private val sqr: SUM[T],
      private val mean: MEAN[T,N])
    (implicit frac: Fractional[T],
      num: Numeric[N], conv: N => T)
  extends Symmetric[T] { self =>
  import frac._

  override def name = "variance"

  protected def pow(a:T) = frac.times(a,a)

  def apply = mean() match {
    case None => None
    case Some(avg) =>  {
      var res = sqr().get
      res = plus(res, times(pow(avg), conv(mean.cnt().get)))

      val midTerm = times(avg, mean.sum().get)
      res = minus(res, midTerm)
      res = minus(res, midTerm)
      Some(res)
    }
  }

  def +(v: T) = new VARIANCE[T,N](sqr + pow(v), mean + num.one)
  def +=(v: T) { sqr += pow(v); mean += v }

  def ++(v: Iterable[T]) = new VARIANCE[T,N]( sqr ++ v.view.map{pow}, mean ++ v )
  def ++=(v: Iterable[T]) { sqr ++= v.map(pow); mean ++= v }
}



class StdDevExact[N]
  (private val varia: VARIANCE[BigDecimal,N])
  extends Symmetric[Double] {

  override def name = "stdDev"

  def apply = varia() match {
    case None => None
    case Some(variance) => variance match {
      case InDoubleRange(doubleVar) => Some(math.sqrt(doubleVar))
      case _ => None
    }
  }

  def +(v: Double) = new StdDevExact(varia + BigDecimal(v))
  def ++(v: Iterable[Double]) = new StdDevExact(varia ++ v.map{BigDecimal(_)})
  def +=(v: Double) { varia += BigDecimal(v) }
  def ++=(v: Iterable[Double]) { varia ++= v.map{BigDecimal(_)} }

  object InDoubleRange {
    def unapply(v: BigDecimal)
    = if (v > BigDecimal(Double.MaxValue))
      None else Some(v.toDouble)
  }
}



object VARIANCE {

  private implicit def long2float(l: Long) = l.toFloat
  private implicit def long2double(l: Long) = l.toDouble
  private implicit def long2bigDec(l: Long) = BigDecimal(l)
  private implicit def bigInt2bigDec(l: BigInt) = BigDecimal(l)

  def forFloat = new VARIANCE(SUM.forFloat, MEAN.forFloat)
  def forDouble = new VARIANCE(SUM.forDouble, MEAN.forDouble)
  // TODO: def forDoubleExact = ...
  def forBigDecimal = new VARIANCE(SUM.forBigDecimal, MEAN.forBigDecimal)

  def standardDeviation = new StdDevExact(forBigDecimal)

  def apply[T](seq: T*)
  (implicit num: Fractional[T], conv: Long => T)
  = {
    val agg = new VARIANCE[T,Long](
      new SUM[T](num.zero),
      new MEAN[T,Long](
        new SUM[T](num.zero),
        SUM.forLong
      )
    )
    agg ++= seq
    agg()
  }
}
