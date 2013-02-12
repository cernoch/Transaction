package cernoch.sm.algebra.aggregators

import collection.immutable.TreeMap

abstract class MEDIAN[T,N]
  (initialMap: TreeMap[T,N])
  (implicit frac: Ordering[T], num: Numeric[N])
extends TreeCounter[T,N](initialMap) {
  import num._

  def name = "median"

  def apply() = crop(map)

  @annotation.tailrec
  private def crop(map:TreeMap[T,N]) : Option[T]
  = map.size match {
    case 0 => None
    case 1 => Some(map.firstKey)
    case _ => {
      val delta = minus(map(map.firstKey), map(map.lastKey))
      if (gt(delta, zero))
        crop(map - map.lastKey + (map.firstKey -> map(map.lastKey)))
      else if (lt(delta, zero))
        crop(map - map.firstKey + (map.lastKey -> map(map.firstKey)))
      else
        if (map.size == 2)
          middleOne(map.firstKey, map.lastKey)
        else
          crop(map - map.firstKey - map.lastKey)
    }
  }

  protected def middleOne(a:T,  b:T): Option[T]
}


object MEDIAN {

  private class NumericToFractional[T]
  (divide: (T,T) => T)
  (implicit num: Numeric[T])
    extends Fractional[T] {

    def div(x: T, y: T): T = divide(x,y)
    def plus(x: T, y: T): T = num.plus(x,y)
    def minus(x: T, y: T): T = num.minus(x,y)
    def times(x: T, y: T): T = num.times(x,y)
    def negate(x: T): T = num.negate(x)
    def fromInt(x: Int): T = num.fromInt(x)
    def toInt(x: T): Int = num.toInt(x)
    def toLong(x: T): Long = num.toLong(x)
    def toFloat(x: T): Float = num.toFloat(x)
    def toDouble(x: T): Double = num.toDouble(x)
    def compare(x: T, y: T): Int = num.compare(x,y)
  }

  def numeric[T](implicit num: Numeric[T]) = num

  def forFloat = new MedianFractional(TreeMap[Float,Long]())
  def forDouble = new MedianFractional(TreeMap[Double,Long]())
  def forBigDecimal = new MedianFractional(TreeMap[BigDecimal,BigInt]())

  def forString = new MedianOrdered(TreeMap[String,Long]())

  def forInt = new MedianFractional( TreeMap[Int,Long]() )( new NumericToFractional[Int]({_ / _}), numeric )
  def forLong = new MedianFractional(TreeMap[Long,Long]() )( new NumericToFractional[Long]({_ / _}), numeric )
  def forBigInt = new MedianFractional(TreeMap[BigInt,BigInt]() )( new NumericToFractional[BigInt]({_ / _}), numeric )

  def apply[T](seq: T*)
  (implicit num: Fractional[T], conv: Long => T)
  : Option[T] = {
    val agg = new MedianFractional(TreeMap[T,Long]())
    agg ++= seq.toIterable
    agg()
  }

  def onOrdered[T](seq: T*)
  (implicit num: Ordering[T])
  : Option[T] = {
    val agg = new MedianOrdered(TreeMap[T, Long]())
    agg ++= seq.toIterable
    agg()
  }
}


class MedianFractional[T,N]
  (map: TreeMap[T,N])
  (implicit frac: Fractional[T], num: Numeric[N])
extends MEDIAN[T,N](map) {
  import frac._

  override def toString() = super.toString + "(fractional)"
  protected def middleOne(a: T, b: T) = Some(div(plus(a,b), plus(one, one)))
  protected def create(map: TreeMap[T, N]) = new MedianFractional[T,N](map)
}


class MedianOrdered[T,N]
  (map: TreeMap[T,N])
  (implicit ord: Ordering[T], num: Numeric[N])
extends MEDIAN[T,N](map) {

  override def toString() = super.toString + "(ordered)"
  protected def middleOne(a: T, b: T) = None
  protected def create(map: TreeMap[T, N]) = new MedianOrdered[T,N](map)
}
