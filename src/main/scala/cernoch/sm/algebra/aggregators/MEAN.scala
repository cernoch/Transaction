package cernoch.sm.algebra.aggregators

class MEAN[T,N]
  ( private[aggregators] var sum: SUM[T],
    private[aggregators] var cnt: SUM[N] )
  ( implicit
    frac: Fractional[T],
    num: Numeric[N],
    conv: N => T )
extends Symmetric[T] {
 import frac._

  def name = "mean"

  def apply() = cnt().get match {
   case 0 => None
   case _ => Some(div(sum().get, conv(cnt().get)))
  }

  def +(v: T) = new MEAN[T,N](sum + v,  cnt + num.one)
  def +=(v: T) { sum += v; cnt += num.one }

  def ++(v: Iterable[T]) = new MEAN[T,N](sum ++ v,  cnt + num.fromInt(v.size))
  def ++=(v: Iterable[T]) { sum ++= v; cnt += num.fromInt(v.size) }
}


object MEAN {

  private implicit def long2float(l: Long) = l.toFloat
  private implicit def long2double(l: Long) = l.toDouble
  private implicit def long2bigDec(l: Long) = BigDecimal(l)
  private implicit def bigInt2bigDec(l: BigInt) = BigDecimal(l)

  def forFloat = new MEAN(SUM.forFloat, SUM.forLong)
  def forDouble = new MEAN(SUM.forDouble, SUM.forLong)
  def forDoubleExact = new MeanDoubleExact(forBigDecimal)
  def forBigDecimal = new MEAN(SUM.forBigDecimal, SUM.forBigInt)

  def apply[T](seq: T*)
  (implicit num: Fractional[T], conv: Long => T)
  = {
    val agg = new MEAN[T,Long](
      new SUM[T](num.zero),
      SUM.forLong )
    agg ++= seq
    agg().get
  }
}


class MeanDoubleExact[N]
  (private[aggregators] val mean: MEAN[BigDecimal,N])
extends Symmetric[Double] {

  def name = "mean"

  def apply() = mean().map(_.toDouble)

  def +(v: Double) = new MeanDoubleExact(mean + BigDecimal(v))
  def +=(v: Double) { mean += BigDecimal(v) }
  def ++(v: Iterable[Double]) = new MeanDoubleExact(mean ++ v.map{BigDecimal(_)})
  def ++=(v: Iterable[Double]) { mean ++= v.map{BigDecimal(_)} }
}
